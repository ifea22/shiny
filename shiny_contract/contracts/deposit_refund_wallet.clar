;; Deposit Refund Wallet Smart Contract
;; Automatically refunds excess deposits when they exceed the maximum balance

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1003))
(define-constant ERR-TRANSFER-FAILED (err u1004))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data variables
(define-data-var max-balance uint u1000000) ;; Default max balance (1,000,000 microSTX)
(define-data-var current-balance uint u0)

;; Read-only functions

;; Get the maximum allowed balance
(define-read-only (get-max-balance)
  (var-get max-balance)
)

;; Get the current wallet balance
(define-read-only (get-current-balance)
  (var-get current-balance)
)

;; Get available deposit space (how much can be deposited without exceeding max)
(define-read-only (get-available-space)
  (let ((max (var-get max-balance))
        (current (var-get current-balance)))
    (if (>= current max)
        u0
        (- max current)
    )
  )
)

;; Check if an amount would exceed the maximum balance
(define-read-only (would-exceed-max (amount uint))
  (let ((current (var-get current-balance))
        (max (var-get max-balance)))
    (> (+ current amount) max)
  )
)

;; Calculate refund amount for a given deposit
(define-read-only (calculate-refund (deposit-amount uint))
  (let ((current (var-get current-balance))
        (max (var-get max-balance))
        (new-total (+ current deposit-amount)))
    (if (> new-total max)
        (- new-total max)
        u0
    )
  )
)

;; Public functions

;; Set the maximum balance (only contract owner)
(define-public (set-max-balance (new-max uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> new-max u0) ERR-INVALID-AMOUNT)
    (var-set max-balance new-max)
    (ok new-max)
  )
)

;; Deposit STX with automatic refund of excess
(define-public (deposit (amount uint))
  (let ((current (var-get current-balance))
        (max (var-get max-balance))
        (new-total (+ current amount))
        (accepted-amount (if (> new-total max) (- max current) amount))
        (refund-amount (if (> new-total max) (- new-total max) u0)))
    
    ;; Validate deposit amount
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer the full deposit amount from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update the balance with only the accepted amount
    (var-set current-balance (+ current accepted-amount))
    
    ;; If there's a refund, send it back to the depositor
    (if (> refund-amount u0)
        (begin
          (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
          (ok {
            deposited: accepted-amount,
            refunded: refund-amount,
            new-balance: (+ current accepted-amount)
          })
        )
        (ok {
          deposited: accepted-amount,
          refunded: u0,
          new-balance: (+ current accepted-amount)
        })
    )
  )
)

;; Withdraw STX (only contract owner)
(define-public (withdraw (amount uint))
  (let ((current (var-get current-balance)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount current) ERR-INSUFFICIENT-BALANCE)
    
    ;; Transfer STX from contract to owner
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    
    ;; Update balance
    (var-set current-balance (- current amount))
    
    (ok {
      withdrawn: amount,
      new-balance: (- current amount)
    })
  )
)

;; Emergency withdraw all funds (only contract owner)
(define-public (emergency-withdraw)
  (let ((current (var-get current-balance)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Transfer all STX from contract to owner
    (try! (as-contract (stx-transfer? current tx-sender CONTRACT-OWNER)))
    
    ;; Reset balance
    (var-set current-balance u0)
    
    (ok {
      withdrawn: current,
      new-balance: u0
    })
  )
)

;; Get contract info
(define-read-only (get-contract-info)
  {
    owner: CONTRACT-OWNER,
    max-balance: (var-get max-balance),
    current-balance: (var-get current-balance),
    available-space: (get-available-space)
  }
)