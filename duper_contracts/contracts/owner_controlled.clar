;; Owner Controlled Wallet Smart Contract
;; A secure wallet contract where only the designated owner can deposit and withdraw funds

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))

;; Data Variables
(define-data-var wallet-balance uint u0)

;; Private Functions
(define-private (is-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

;; Public Functions

;; Deposit function - only owner can deposit STX
(define-public (deposit (amount uint))
    (begin
        ;; Check if caller is the owner
        (asserts! (is-owner) ERR_UNAUTHORIZED)
        
        ;; Check if amount is greater than 0
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        ;; Transfer STX from owner to contract
        (match (stx-transfer? amount tx-sender (as-contract tx-sender))
            success (begin
                ;; Update wallet balance
                (var-set wallet-balance (+ (var-get wallet-balance) amount))
                (ok amount)
            )
            error (err error)
        )
    )
)

;; Withdraw function - only owner can withdraw STX
(define-public (withdraw (amount uint))
    (begin
        ;; Check if caller is the owner
        (asserts! (is-owner) ERR_UNAUTHORIZED)
        
        ;; Check if amount is greater than 0
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        ;; Check if contract has sufficient balance
        (asserts! (>= (var-get wallet-balance) amount) ERR_INSUFFICIENT_BALANCE)
        
        ;; Transfer STX from contract to owner
        (match (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER))
            success (begin
                ;; Update wallet balance
                (var-set wallet-balance (- (var-get wallet-balance) amount))
                (ok amount)
            )
            error (err error)
        )
    )
)

;; Read-only Functions

;; Get the contract owner
(define-read-only (get-owner)
    CONTRACT_OWNER
)

;; Get current wallet balance
(define-read-only (get-balance)
    (var-get wallet-balance)
)

;; Get actual STX balance of the contract
(define-read-only (get-contract-stx-balance)
    (stx-get-balance (as-contract tx-sender))
)

;; Check if an address is the owner
(define-read-only (is-contract-owner (address principal))
    (is-eq address CONTRACT_OWNER)
)