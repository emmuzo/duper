;; Blacklist with Penalty Wallet Smart Contract
;; This contract manages a blacklist system with fund locking penalties

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_USER_NOT_BLACKLISTED (err u101))
(define-constant ERR_USER_ALREADY_BLACKLISTED (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_FUNDS_STILL_LOCKED (err u104))
(define-constant ERR_NO_LOCKED_FUNDS (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))

;; Data Variables
(define-data-var contract-admin principal CONTRACT_OWNER)

;; Data Maps
;; Track blacklisted users with penalty details
(define-map blacklisted-users 
    principal 
    {
        is-blacklisted: bool,
        penalty-amount: uint,
        lock-end-block: uint,
        blacklist-reason: (string-ascii 100)
    }
)

;; Track user balances (for demonstration purposes)
(define-map user-balances principal uint)

;; Track locked funds
(define-map locked-funds principal uint)

;; Public Functions

;; Initialize user balance (for testing purposes)
(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (map-set user-balances tx-sender 
            (+ (default-to u0 (map-get? user-balances tx-sender)) amount))
        (ok amount)
    )
)

;; Add user to blacklist with penalty
(define-public (blacklist-user 
    (user principal) 
    (penalty-amount uint) 
    (lock-duration-blocks uint)
    (reason (string-ascii 100)))
    (let (
        (current-balance (default-to u0 (map-get? user-balances user)))
        (current-block-height block-height)
    )
        ;; Only admin can blacklist users
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_UNAUTHORIZED)
        
        ;; Check if user is already blacklisted
        (asserts! (is-none (map-get? blacklisted-users user)) ERR_USER_ALREADY_BLACKLISTED)
        
        ;; Ensure user has sufficient balance for penalty
        (asserts! (>= current-balance penalty-amount) ERR_INSUFFICIENT_BALANCE)
        
        ;; Lock the penalty amount
        (map-set locked-funds user penalty-amount)
        
        ;; Update user balance (subtract locked funds)
        (map-set user-balances user (- current-balance penalty-amount))
        
        ;; Add user to blacklist
        (map-set blacklisted-users user {
            is-blacklisted: true,
            penalty-amount: penalty-amount,
            lock-end-block: (+ current-block-height lock-duration-blocks),
            blacklist-reason: reason
        })
        
        (ok true)
    )
)

;; Remove user from blacklist (admin only)
(define-public (remove-from-blacklist (user principal))
    (let (
        (blacklist-info (unwrap! (map-get? blacklisted-users user) ERR_USER_NOT_BLACKLISTED))
    )
        ;; Only admin can remove from blacklist
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_UNAUTHORIZED)
        
        ;; Remove from blacklist
        (map-delete blacklisted-users user)
        
        ;; If lock period has ended, return locked funds
        (if (>= block-height (get lock-end-block blacklist-info))
            (begin
                (map-set user-balances user 
                    (+ (default-to u0 (map-get? user-balances user)) 
                       (get penalty-amount blacklist-info)))
                (map-delete locked-funds user)
            )
            ;; If still locked, keep funds locked but remove blacklist status
            true
        )
        
        (ok true)
    )
)

;; Claim locked funds after lock period expires
(define-public (claim-locked-funds)
    (let (
        (blacklist-info (map-get? blacklisted-users tx-sender))
        (locked-amount (default-to u0 (map-get? locked-funds tx-sender)))
    )
        ;; Check if user has locked funds
        (asserts! (> locked-amount u0) ERR_NO_LOCKED_FUNDS)
        
        ;; If user is still blacklisted, check if lock period has ended
        (match blacklist-info
            some-info (asserts! (>= block-height (get lock-end-block some-info)) ERR_FUNDS_STILL_LOCKED)
            true ;; User not blacklisted, can claim funds
        )
        
        ;; Return locked funds to user balance
        (map-set user-balances tx-sender 
            (+ (default-to u0 (map-get? user-balances tx-sender)) locked-amount))
        
        ;; Remove locked funds record
        (map-delete locked-funds tx-sender)
        
        (ok locked-amount)
    )
)

;; Withdraw available balance (non-locked funds)
(define-public (withdraw (amount uint))
    (let (
        (current-balance (default-to u0 (map-get? user-balances tx-sender)))
        (blacklist-info (map-get? blacklisted-users tx-sender))
    )
        ;; Check if user is blacklisted and if so, prevent withdrawals
        (match blacklist-info
            some-info (asserts! (not (get is-blacklisted some-info)) ERR_UNAUTHORIZED)
            true ;; User not blacklisted, can withdraw
        )
        
        ;; Ensure sufficient balance
        (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        ;; Update balance
        (map-set user-balances tx-sender (- current-balance amount))
        
        (ok amount)
    )
)

;; Transfer admin rights
(define-public (transfer-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-admin)) ERR_UNAUTHORIZED)
        (var-set contract-admin new-admin)
        (ok true)
    )
)

;; Read-only Functions

;; Check if user is blacklisted
(define-read-only (is-blacklisted (user principal))
    (match (map-get? blacklisted-users user)
        some-info (get is-blacklisted some-info)
        false
    )
)

;; Get blacklist information for a user
(define-read-only (get-blacklist-info (user principal))
    (map-get? blacklisted-users user)
)

;; Get user balance
(define-read-only (get-balance (user principal))
    (default-to u0 (map-get? user-balances user))
)

;; Get locked funds amount
(define-read-only (get-locked-funds (user principal))
    (default-to u0 (map-get? locked-funds user))
)

;; Get contract admin
(define-read-only (get-admin)
    (var-get contract-admin)
)

;; Check if funds can be claimed (lock period ended)
(define-read-only (can-claim-funds (user principal))
    (let (
        (blacklist-info (map-get? blacklisted-users user))
        (has-locked-funds (> (default-to u0 (map-get? locked-funds user)) u0))
    )
        (and 
            has-locked-funds
            (match blacklist-info
                some-info (>= block-height (get lock-end-block some-info))
                true ;; No blacklist info means can claim if has locked funds
            )
        )
    )
)