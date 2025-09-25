;; Multi-Send Contract with Recipient Limit
;; Allows sending STX to multiple recipients with gas optimization controls

;; Constants
(define-constant MAX_RECIPIENTS u50) ;; Maximum number of recipients per transaction
(define-constant CONTRACT_OWNER tx-sender)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u1000))
(define-constant ERR_INVALID_AMOUNT (err u1001))
(define-constant ERR_TOO_MANY_RECIPIENTS (err u1002))
(define-constant ERR_EMPTY_RECIPIENTS (err u1003))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1004))
(define-constant ERR_TRANSFER_FAILED (err u1005))
(define-constant ERR_INVALID_RECIPIENT (err u1006))

;; Data variables
(define-data-var max-recipients-limit uint MAX_RECIPIENTS)
(define-data-var total-transactions uint u0)

;; Data maps
(define-map user-transaction-count principal uint)

;; Read-only functions

;; Get the current maximum recipients limit
(define-read-only (get-max-recipients-limit)
  (var-get max-recipients-limit)
)

;; Get total number of multi-send transactions processed
(define-read-only (get-total-transactions)
  (var-get total-transactions)
)

;; Get transaction count for a specific user
(define-read-only (get-user-transaction-count (user principal))
  (default-to u0 (map-get? user-transaction-count user))
)

;; Calculate total amount needed for a list of recipients
(define-read-only (calculate-total-amount (recipients (list 50 {recipient: principal, amount: uint})))
  (fold calculate-sum recipients u0)
)

;; Helper function for calculating sum
(define-private (calculate-sum (recipient-data {recipient: principal, amount: uint}) (acc uint))
  (+ acc (get amount recipient-data))
)

;; Validate recipients list
(define-read-only (validate-recipients (recipients (list 50 {recipient: principal, amount: uint})))
  (let ((recipients-count (len recipients)))
    (and 
      (> recipients-count u0) ;; Not empty
      (<= recipients-count (var-get max-recipients-limit)) ;; Within limit
      (> (calculate-total-amount recipients) u0) ;; Total amount > 0
    )
  )
)

;; Public functions

;; Main multi-send function
(define-public (multi-send (recipients (list 50 {recipient: principal, amount: uint})))
  (let (
    (recipients-count (len recipients))
    (total-amount (calculate-total-amount recipients))
    (sender-balance (stx-get-balance tx-sender))
  )
    ;; Validate inputs
    (asserts! (> recipients-count u0) ERR_EMPTY_RECIPIENTS)
    (asserts! (<= recipients-count (var-get max-recipients-limit)) ERR_TOO_MANY_RECIPIENTS)
    (asserts! (> total-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= sender-balance total-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Execute transfers
    (match (fold send-to-recipient recipients (ok u0))
      success (begin
        ;; Update statistics
        (var-set total-transactions (+ (var-get total-transactions) u1))
        (map-set user-transaction-count 
          tx-sender 
          (+ (get-user-transaction-count tx-sender) u1))
        (ok {
          recipients-count: recipients-count,
          total-amount: total-amount,
          transaction-id: (var-get total-transactions)
        })
      )
      error (err error)
    )
  )
)

;; Helper function to send STX to individual recipient
(define-private (send-to-recipient 
  (recipient-data {recipient: principal, amount: uint}) 
  (previous-result (response uint uint)))
  (match previous-result
    success (begin
      ;; Validate recipient and amount
      (asserts! (is-standard (get recipient recipient-data)) ERR_INVALID_RECIPIENT)
      (asserts! (> (get amount recipient-data) u0) ERR_INVALID_AMOUNT)
      
      ;; Execute transfer
      (match (stx-transfer? (get amount recipient-data) tx-sender (get recipient recipient-data))
        transfer-success (ok (+ success (get amount recipient-data)))
        transfer-error ERR_TRANSFER_FAILED
      )
    )
    error (err error)
  )
)

;; Admin function to update max recipients limit
(define-public (set-max-recipients-limit (new-limit uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (and (> new-limit u0) (<= new-limit u100)) ERR_INVALID_AMOUNT)
    (var-set max-recipients-limit new-limit)
    (ok new-limit)
  )
)

;; Batch send with equal amounts
(define-public (multi-send-equal (recipients (list 50 principal)) (amount-per-recipient uint))
  (let (
    (recipients-count (len recipients))
    (total-amount (* recipients-count amount-per-recipient))
  )
    ;; Validate inputs
    (asserts! (> recipients-count u0) ERR_EMPTY_RECIPIENTS)
    (asserts! (<= recipients-count (var-get max-recipients-limit)) ERR_TOO_MANY_RECIPIENTS)
    (asserts! (> amount-per-recipient u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Execute equal transfers
    (match (fold send-equal-amount recipients (ok {amount: amount-per-recipient, count: u0}))
      success (begin
        ;; Update statistics
        (var-set total-transactions (+ (var-get total-transactions) u1))
        (map-set user-transaction-count 
          tx-sender 
          (+ (get-user-transaction-count tx-sender) u1))
        (ok {
          recipients-count: recipients-count,
          amount-per-recipient: amount-per-recipient,
          total-amount: total-amount,
          transaction-id: (var-get total-transactions)
        })
      )
      error (err error)
    )
  )
)

;; Helper function for equal amount transfers
(define-private (send-equal-amount 
  (recipient principal) 
  (previous-result (response {amount: uint, count: uint} uint)))
  (match previous-result
    success (begin
      ;; Validate recipient
      (asserts! (is-standard recipient) ERR_INVALID_RECIPIENT)
      
      ;; Execute transfer
      (match (stx-transfer? (get amount success) tx-sender recipient)
        transfer-success (ok {
          amount: (get amount success),
          count: (+ (get count success) u1)
        })
        transfer-error ERR_TRANSFER_FAILED
      )
    )
    error (err error)
  )
)

;; Emergency function to get contract info
(define-read-only (get-contract-info)
  {
    max-recipients-limit: (var-get max-recipients-limit),
    total-transactions: (var-get total-transactions),
    contract-owner: CONTRACT_OWNER
  }
)