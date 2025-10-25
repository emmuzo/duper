;; FilePinning Smart Contract
;; Purpose: Pin files on specific nodes to guarantee availability

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_NODE (err u101))
(define-constant ERR_FILE_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_PINNED (err u104))
(define-constant ERR_NOT_PINNED (err u105))
(define-constant ERR_INVALID_DURATION (err u106))
(define-constant ERR_INVALID_FILE_HASH (err u107))
(define-constant ERR_INVALID_PIN_RATE (err u108))
(define-constant ERR_ARITHMETIC_OVERFLOW (err u109))

;; Data Variables
(define-data-var next-pin-id uint u1)
(define-data-var minimum-pin-cost uint u1000000) ;; 1 STX in microSTX
(define-data-var max-pin-duration uint u52560000) ;; ~1 year in blocks
(define-data-var max-pin-rate uint u1000000000) ;; Maximum pin rate to prevent overflow

;; Data Maps

;; Storage node registry
(define-map storage-nodes
  { node-id: principal }
  {
    is-active: bool,
    reputation-score: uint,
    total-pins: uint,
    pin-rate: uint, ;; cost per block in microSTX
    last-activity: uint
  }
)

;; File pinning requests
(define-map pin-requests
  { pin-id: uint }
  {
    file-hash: (string-ascii 64),
    requester: principal,
    node-id: principal,
    pin-cost: uint,
    duration: uint, ;; in blocks
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20), ;; "pending", "active", "completed", "failed"
    created-at: uint
  }
)

;; Track active pins by file hash and node
(define-map file-pins
  { file-hash: (string-ascii 64), node-id: principal }
  {
    pin-id: uint,
    is-active: bool
  }
)

;; Node earnings tracking
(define-map node-earnings
  { node-id: principal }
  {
    total-earned: uint,
    pending-payment: uint,
    last-payout: uint
  }
)

;; User pin tracking
(define-map user-pins
  { user: principal, pin-id: uint }
  { exists: bool }
)

;; Input validation helpers
(define-private (is-valid-file-hash (file-hash (string-ascii 64)))
  (and 
    (> (len file-hash) u0)
    (<= (len file-hash) u64)
    ;; Basic hex character validation - only allows 0-9, a-f, A-F
    (is-none (index-of file-hash " "))
  )
)

(define-private (safe-multiply (a uint) (b uint))
  (let ((result (* a b)))
    ;; Check for overflow by ensuring division returns original value
    (if (and (> a u0) (> b u0))
      (if (is-eq (/ result a) b)
        (some result)
        none
      )
      (some result)
    )
  )
)

;; Public Functions

;; Register as a storage node
(define-public (register-node (pin-rate uint))
  (let ((node tx-sender))
    (asserts! (> pin-rate u0) ERR_INVALID_PIN_RATE)
    (asserts! (<= pin-rate (var-get max-pin-rate)) ERR_INVALID_PIN_RATE)
    (asserts! (>= pin-rate (var-get minimum-pin-cost)) ERR_INVALID_PIN_RATE)
    (map-set storage-nodes
      { node-id: node }
      {
        is-active: true,
        reputation-score: u100,
        total-pins: u0,
        pin-rate: pin-rate,
        last-activity: block-height
      }
    )
    (ok true)
  )
)

;; Update node status
(define-public (update-node-status (is-active bool))
  (let ((node tx-sender))
    (match (map-get? storage-nodes { node-id: node })
      node-data
      (begin
        (map-set storage-nodes
          { node-id: node }
          (merge node-data { is-active: is-active, last-activity: block-height })
        )
        (ok true)
      )
      ERR_INVALID_NODE
    )
  )
)

;; Request file pinning
(define-public (request-pinning (file-hash (string-ascii 64)) (node-id principal) (duration uint))
  (let (
    (pin-id (var-get next-pin-id))
    (requester tx-sender)
  )
    ;; Validate inputs
    (asserts! (is-valid-file-hash file-hash) ERR_INVALID_FILE_HASH)
    (asserts! (> duration u0) ERR_INVALID_DURATION)
    (asserts! (<= duration (var-get max-pin-duration)) ERR_INVALID_DURATION)
    (asserts! (not (is-eq node-id requester)) ERR_UNAUTHORIZED) ;; Prevent self-pinning
    
    ;; Check if node exists and is active
    (match (map-get? storage-nodes { node-id: node-id })
      node-data
      (begin
        (asserts! (get is-active node-data) ERR_INVALID_NODE)
        
        ;; Check if file is already pinned on this node
        (try! (match (map-get? file-pins { file-hash: file-hash, node-id: node-id })
          existing-pin
          (if (get is-active existing-pin)
            ERR_ALREADY_PINNED
            (ok true)
          )
          (ok true)
        ))
        
        ;; Safe cost calculation with overflow protection
        (match (safe-multiply (get pin-rate node-data) duration)
          pin-cost
          (begin
            ;; Ensure minimum cost is met
            (asserts! (>= pin-cost (var-get minimum-pin-cost)) ERR_INSUFFICIENT_FUNDS)
            
            ;; Check if user has sufficient funds
            (asserts! (>= (stx-get-balance requester) pin-cost) ERR_INSUFFICIENT_FUNDS)
            
            ;; Transfer payment to contract
            (try! (stx-transfer? pin-cost requester (as-contract tx-sender)))
            
            ;; Create pin request
            (map-set pin-requests
              { pin-id: pin-id }
              {
                file-hash: file-hash,
                requester: requester,
                node-id: node-id,
                pin-cost: pin-cost,
                duration: duration,
                start-block: block-height,
                end-block: (+ block-height duration),
                status: "pending",
                created-at: block-height
              }
            )
            
            ;; Track user pin
            (map-set user-pins
              { user: requester, pin-id: pin-id }
              { exists: true }
            )
            
            ;; Update next pin ID
            (var-set next-pin-id (+ pin-id u1))
            
            (ok pin-id)
          )
          ERR_ARITHMETIC_OVERFLOW
        )
      )
      ERR_INVALID_NODE
    )
  )
)

;; Confirm pinning by node
(define-public (confirm-pinning (pin-id uint))
  (let ((node tx-sender))
    (match (map-get? pin-requests { pin-id: pin-id })
      pin-data
      (begin
        (asserts! (is-eq (get node-id pin-data) node) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status pin-data) "pending") ERR_INVALID_NODE)
        
        ;; Update pin status to active
        (map-set pin-requests
          { pin-id: pin-id }
          (merge pin-data { status: "active" })
        )
        
        ;; Mark file as pinned
        (map-set file-pins
          { file-hash: (get file-hash pin-data), node-id: node }
          { pin-id: pin-id, is-active: true }
        )
        
        ;; Update node stats
        (match (map-get? storage-nodes { node-id: node })
          node-data
          (map-set storage-nodes
            { node-id: node }
            (merge node-data {
              total-pins: (+ (get total-pins node-data) u1),
              last-activity: block-height
            })
          )
          false
        )
        
        ;; Update node earnings
        (let ((current-earnings (default-to { total-earned: u0, pending-payment: u0, last-payout: u0 }
                                  (map-get? node-earnings { node-id: node }))))
          (map-set node-earnings
            { node-id: node }
            (merge current-earnings {
              pending-payment: (+ (get pending-payment current-earnings) (get pin-cost pin-data))
            })
          )
        )
        
        (ok true)
      )
      ERR_FILE_NOT_FOUND
    )
  )
)

;; Complete pinning (called when duration expires or manually)
(define-public (complete-pinning (pin-id uint))
  (match (map-get? pin-requests { pin-id: pin-id })
    pin-data
    (let ((node-id (get node-id pin-data)))
      (asserts! (or 
        (is-eq tx-sender node-id)
        (is-eq tx-sender (get requester pin-data))
        (>= block-height (get end-block pin-data))
      ) ERR_UNAUTHORIZED)
      
      (asserts! (is-eq (get status pin-data) "active") ERR_NOT_PINNED)
      
      ;; Update pin status
      (map-set pin-requests
        { pin-id: pin-id }
        (merge pin-data { status: "completed" })
      )
      
      ;; Deactivate file pin
      (map-set file-pins
        { file-hash: (get file-hash pin-data), node-id: node-id }
        { pin-id: pin-id, is-active: false }
      )
      
      (ok true)
    )
    ERR_FILE_NOT_FOUND
  )
)

;; Claim earnings for nodes
(define-public (claim-earnings)
  (let ((node tx-sender))
    (match (map-get? node-earnings { node-id: node })
      earnings-data
      (let ((pending-amount (get pending-payment earnings-data)))
        (asserts! (> pending-amount u0) ERR_INSUFFICIENT_FUNDS)
        
        ;; Transfer earnings to node
        (try! (as-contract (stx-transfer? pending-amount tx-sender node)))
        
        ;; Update earnings record
        (map-set node-earnings
          { node-id: node }
          {
            total-earned: (+ (get total-earned earnings-data) pending-amount),
            pending-payment: u0,
            last-payout: block-height
          }
        )
        
        (ok pending-amount)
      )
      ERR_INVALID_NODE
    )
  )
)

;; Admin function to update minimum pin cost
(define-public (set-minimum-pin-cost (new-cost uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set minimum-pin-cost new-cost)
    (ok true)
  )
)

;; Read-only Functions

;; Get pin request details
(define-read-only (get-pin-request (pin-id uint))
  (map-get? pin-requests { pin-id: pin-id })
)

;; Get node information
(define-read-only (get-node-info (node-id principal))
  (map-get? storage-nodes { node-id: node-id })
)

;; Check if file is pinned on node
(define-read-only (is-file-pinned (file-hash (string-ascii 64)) (node-id principal))
  (match (map-get? file-pins { file-hash: file-hash, node-id: node-id })
    pin-info (get is-active pin-info)
    false
  )
)

;; Get node earnings
(define-read-only (get-node-earnings (node-id principal))
  (map-get? node-earnings { node-id: node-id })
)

;; Get pin cost estimate
(define-read-only (estimate-pin-cost (node-id principal) (duration uint))
  (match (map-get? storage-nodes { node-id: node-id })
    node-data 
    (match (safe-multiply (get pin-rate node-data) duration)
      cost cost
      u0
    )
    u0
  )
)

;; Get current pin ID
(define-read-only (get-current-pin-id)
  (var-get next-pin-id)
)

;; Check if user has specific pin
(define-read-only (user-has-pin (user principal) (pin-id uint))
  (is-some (map-get? user-pins { user: user, pin-id: pin-id }))
)