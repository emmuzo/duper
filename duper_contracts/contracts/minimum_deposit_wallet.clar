;; Minimum Deposit Wallet Smart Contract
;; This contract enforces a minimum deposit amount and rejects smaller deposits

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_DEPOSIT_TOO_SMALL (err u402))
(define-constant ERR_INSUFFICIENT_BALANCE (err u403))
(define-constant ERR_INVALID_AMOUNT (err u404))

;; Data Variables
(define-data-var minimum-deposit uint u1000000) ;; Default 1 STX (1,000,000 microSTX)

;; Data Maps
(define-map user-balances principal uint)

;; Read-only functions
(define-read-only (get-minimum-deposit)
  (var-get minimum-deposit)
)

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Public functions

;; Set minimum deposit amount (only contract owner)
(define-public (set-minimum-deposit (new-minimum uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-minimum u0) ERR_INVALID_AMOUNT)
    (var-set minimum-deposit new-minimum)
    (ok new-minimum)
  )
)

;; Deposit STX with minimum amount enforcement
(define-public (deposit (amount uint))
  (let (
    (current-balance (get-balance tx-sender))
    (min-deposit (var-get minimum-deposit))
  )
    ;; Check if deposit meets minimum requirement
    (asserts! (>= amount min-deposit) ERR_DEPOSIT_TOO_SMALL)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update user balance
    (map-set user-balances tx-sender (+ current-balance amount))
    
    ;; Return success with new balance
    (ok (+ current-balance amount))
  )
)

;; Withdraw STX from user's balance
(define-public (withdraw (amount uint))
  (let (
    (current-balance (get-balance tx-sender))
  )
    ;; Check if user has sufficient balance
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Update user balance first
    (map-set user-balances tx-sender (- current-balance amount))
    
    ;; Transfer STX from contract to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Return success with remaining balance
    (ok (- current-balance amount))
  )
)

;; Withdraw all funds from user's balance
(define-public (withdraw-all)
  (let (
    (current-balance (get-balance tx-sender))
  )
    (asserts! (> current-balance u0) ERR_INSUFFICIENT_BALANCE)
    
    ;; Clear user balance
    (map-delete user-balances tx-sender)
    
    ;; Transfer all STX from contract to user
    (try! (as-contract (stx-transfer? current-balance tx-sender tx-sender)))
    
    ;; Return success
    (ok current-balance)
  )
)

;; Emergency function for contract owner to withdraw contract funds
(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer STX from contract to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    (ok amount)
  )
)