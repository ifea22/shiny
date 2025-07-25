;; StorageExpiration Smart Contract
;; Handles file expiration, notifications, and archival

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_FILE_NOT_FOUND (err u101))
(define-constant ERR_FILE_EXPIRED (err u102))
(define-constant ERR_INVALID_EXPIRATION (err u103))
(define-constant ERR_ALREADY_ARCHIVED (err u104))
(define-constant ERR_NOTIFICATION_FAILED (err u105))

;; Data variables
(define-data-var next-file-id uint u1)
(define-data-var notification-threshold uint u86400) ;; 24 hours in seconds
(define-data-var auto-archive-enabled bool true)

;; File status enum
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_EXPIRING_SOON u2)
(define-constant STATUS_EXPIRED u3)
(define-constant STATUS_ARCHIVED u4)
(define-constant STATUS_DELETED u5)

;; Data maps
(define-map files
  { file-id: uint }
  {
    owner: principal,
    file-hash: (string-ascii 64),
    file-name: (string-ascii 256),
    file-size: uint,
    created-at: uint,
    expires-at: uint,
    status: uint,
    archived-at: (optional uint),
    notification-sent: bool
  }
)

(define-map owner-files
  { owner: principal, file-id: uint }
  { active: bool }
)

(define-map expiration-queue
  { expires-at: uint, file-id: uint }
  { queued: bool }
)

;; Private functions

;; Check if file has expired
(define-private (is-expired (expires-at uint))
  (> block-height expires-at)
)

;; Check if file is expiring soon
(define-private (is-expiring-soon (expires-at uint))
  (and 
    (not (is-expired expires-at))
    (<= (- expires-at block-height) (var-get notification-threshold))
  )
)

;; Update file status based on expiration
(define-private (update-file-status (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data 
      (let ((expires-at (get expires-at file-data))
            (current-status (get status file-data)))
        (if (is-expired expires-at)
          (if (is-eq current-status STATUS_ACTIVE)
            (ok (map-set files 
              { file-id: file-id }
              (merge file-data { status: STATUS_EXPIRED })
            ))
            (ok true)
          )
          (if (and (is-expiring-soon expires-at) (is-eq current-status STATUS_ACTIVE))
            (ok (map-set files 
              { file-id: file-id }
              (merge file-data { status: STATUS_EXPIRING_SOON })
            ))
            (ok true)
          )
        )
      )
    ERR_FILE_NOT_FOUND
  )
)

;; Archive file
(define-private (archive-file-internal (file-id uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    (if (or (is-eq (get status file-data) STATUS_ARCHIVED) 
            (is-eq (get status file-data) STATUS_DELETED))
      ERR_ALREADY_ARCHIVED
      (ok (map-set files 
        { file-id: file-id }
        (merge file-data { 
          status: STATUS_ARCHIVED,
          archived-at: (some block-height)
        })
      ))
    )
  )
)

;; Delete file permanently
(define-private (delete-file-internal (file-id uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    (begin
      (map-set files 
        { file-id: file-id }
        (merge file-data { status: STATUS_DELETED })
      )
      (map-delete owner-files { owner: (get owner file-data), file-id: file-id })
      (map-delete expiration-queue { expires-at: (get expires-at file-data), file-id: file-id })
      (ok true)
    )
  )
)

;; Public functions

;; Store a new file with expiration
(define-public (store-file 
  (file-hash (string-ascii 64))
  (file-name (string-ascii 256))
  (file-size uint)
  (expires-in-blocks uint))
  (let ((file-id (var-get next-file-id))
        (expires-at (+ block-height expires-in-blocks)))
    (if (< expires-in-blocks u1)
      ERR_INVALID_EXPIRATION
      (begin
        (map-set files 
          { file-id: file-id }
          {
            owner: tx-sender,
            file-hash: file-hash,
            file-name: file-name,
            file-size: file-size,
            created-at: block-height,
            expires-at: expires-at,
            status: STATUS_ACTIVE,
            archived-at: none,
            notification-sent: false
          }
        )
        (map-set owner-files 
          { owner: tx-sender, file-id: file-id }
          { active: true }
        )
        (map-set expiration-queue 
          { expires-at: expires-at, file-id: file-id }
          { queued: true }
        )
        (var-set next-file-id (+ file-id u1))
        (ok file-id)
      )
    )
  )
)

;; Extend file expiration
(define-public (extend-expiration (file-id uint) (additional-blocks uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    (if (not (is-eq tx-sender (get owner file-data)))
      ERR_UNAUTHORIZED
      (if (is-eq (get status file-data) STATUS_DELETED)
        ERR_FILE_NOT_FOUND
        (let ((old-expires-at (get expires-at file-data))
              (new-expires-at (+ (get expires-at file-data) additional-blocks)))
          (begin
            ;; Remove from old expiration queue
            (map-delete expiration-queue { expires-at: old-expires-at, file-id: file-id })
            ;; Update file with new expiration
            (map-set files 
              { file-id: file-id }
              (merge file-data { 
                expires-at: new-expires-at,
                status: STATUS_ACTIVE,
                notification-sent: false
              })
            )
            ;; Add to new expiration queue
            (map-set expiration-queue 
              { expires-at: new-expires-at, file-id: file-id }
              { queued: true }
            )
            (ok new-expires-at)
          )
        )
      )
    )
  )
)

;; Check and update file status
(define-public (check-file-expiration (file-id uint))
  (begin
    (try! (update-file-status file-id))
    (ok (get-file-info file-id))
  )
)

;; Send expiration notification (mock implementation)
(define-public (send-expiration-notification (file-id uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    (if (and 
          (is-expiring-soon (get expires-at file-data))
          (not (get notification-sent file-data)))
      (begin
        (map-set files 
          { file-id: file-id }
          (merge file-data { notification-sent: true })
        )
        (print { 
          event: "expiration-notification",
          file-id: file-id,
          owner: (get owner file-data),
          expires-at: (get expires-at file-data),
          file-name: (get file-name file-data)
        })
        (ok true)
      )
      ERR_NOTIFICATION_FAILED
    )
  )
)

;; Archive expired file
(define-public (archive-file (file-id uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    (if (not (is-eq tx-sender (get owner file-data)))
      ERR_UNAUTHORIZED
      (if (is-expired (get expires-at file-data))
        (archive-file-internal file-id)
        ERR_FILE_NOT_FOUND
      )
    )
  )
)

;; Delete file (only owner or after expiration)
(define-public (delete-file (file-id uint))
  (let ((file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    (if (and 
          (not (is-eq tx-sender (get owner file-data)))
          (not (is-expired (get expires-at file-data))))
      ERR_UNAUTHORIZED
      (delete-file-internal file-id)
    )
  )
)

;; Batch process expiring files
(define-public (process-expired-files (file-ids (list 50 uint)))
  (if (not (is-eq tx-sender CONTRACT_OWNER))
    ERR_UNAUTHORIZED
    (ok (map process-single-expired-file file-ids))
  )
)

;; Process single expired file
(define-private (process-single-expired-file (file-id uint))
  (match (map-get? files { file-id: file-id })
    file-data 
      (if (is-expired (get expires-at file-data))
        (if (var-get auto-archive-enabled)
          (match (archive-file-internal file-id)
            success true
            error false
          )
          (match (delete-file-internal file-id)
            success true
            error false
          )
        )
        false
      )
    false
  )
)

;; Read-only functions

;; Get file information
(define-read-only (get-file-info (file-id uint))
  (map-get? files { file-id: file-id })
)

;; Get file ownership status
(define-read-only (is-file-owner (file-id uint) (owner principal))
  (match (map-get? owner-files { owner: owner, file-id: file-id })
    entry (get active entry)
    false
  )
)

;; Check if file is active for owner
(define-read-only (get-file-status-for-owner (file-id uint) (owner principal))
  (match (map-get? files { file-id: file-id })
    file-data 
      (if (and 
            (is-eq (get owner file-data) owner)
            (not (is-eq (get status file-data) STATUS_DELETED)))
        (some {
          file-id: file-id,
          status: (get status file-data),
          expires-at: (get expires-at file-data),
          file-name: (get file-name file-data)
        })
        none
      )
    none
  )
)

;; Get expiring files (mock - would need iteration in practice)
(define-read-only (get-expiring-files-count)
  u0 ;; Placeholder - would implement with contract iteration
)

;; Get contract settings
(define-read-only (get-contract-settings)
  {
    notification-threshold: (var-get notification-threshold),
    auto-archive-enabled: (var-get auto-archive-enabled),
    next-file-id: (var-get next-file-id)
  }
)

;; Admin functions

;; Update notification threshold
(define-public (set-notification-threshold (new-threshold uint))
  (if (not (is-eq tx-sender CONTRACT_OWNER))
    ERR_UNAUTHORIZED
    (ok (var-set notification-threshold new-threshold))
  )
)

;; Toggle auto-archive
(define-public (set-auto-archive (enabled bool))
  (if (not (is-eq tx-sender CONTRACT_OWNER))
    ERR_UNAUTHORIZED
    (ok (var-set auto-archive-enabled enabled))
  )
)

;; Emergency pause (set all files to archived status)
(define-public (emergency-pause)
  (if (not (is-eq tx-sender CONTRACT_OWNER))
    ERR_UNAUTHORIZED
    (ok true) ;; Would implement batch status update
  )
)