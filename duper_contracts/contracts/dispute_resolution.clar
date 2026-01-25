;; DisputeResolution Smart Contract
;; Purpose: Handle disputes between users and storage nodes

;; Constants
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_DISPUTE (err u400))
(define-constant ERR_DISPUTE_ALREADY_EXISTS (err u409))
(define-constant ERR_DISPUTE_ALREADY_RESOLVED (err u410))
(define-constant ERR_INVALID_VOTE (err u411))
(define-constant ERR_VOTING_PERIOD_ENDED (err u412))
(define-constant ERR_INSUFFICIENT_STAKE (err u413))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Configuration constants
(define-constant VOTING_PERIOD u144) ;; ~24 hours in blocks (10 min/block)
(define-constant MIN_ARBITRATOR_STAKE u1000000) ;; 1 STX minimum stake
(define-constant DISPUTE_FEE u100000) ;; 0.1 STX fee to file dispute
(define-constant QUORUM_THRESHOLD u3) ;; Minimum votes needed for resolution

;; Dispute status enum
(define-constant DISPUTE_PENDING u0)
(define-constant DISPUTE_RESOLVED_FOR_USER u1)
(define-constant DISPUTE_RESOLVED_FOR_NODE u2)
(define-constant DISPUTE_CANCELLED u3)

;; Complaint types
(define-constant COMPLAINT_MISSING_FILE u0)
(define-constant COMPLAINT_CORRUPTED_FILE u1)
(define-constant COMPLAINT_UNAVAILABLE_NODE u2)
(define-constant COMPLAINT_UNAUTHORIZED_ACCESS u3)

;; Data structures
(define-map disputes
  { dispute-id: uint }
  {
    complainant: principal,
    accused-node: principal,
    complaint-type: uint,
    file-hash: (string-ascii 64),
    description: (string-ascii 500),
    evidence-hash: (string-ascii 64),
    status: uint,
    created-at: uint,
    voting-end: uint,
    votes-for-user: uint,
    votes-for-node: uint,
    total-stake-for-user: uint,
    total-stake-for-node: uint,
    refund-amount: uint
  }
)

(define-map arbitrators
  { arbitrator: principal }
  {
    stake: uint,
    reputation: uint,
    active: bool,
    total-votes: uint,
    correct-votes: uint
  }
)

(define-map dispute-votes
  { dispute-id: uint, arbitrator: principal }
  {
    vote: uint, ;; 0 = no vote, 1 = for user, 2 = for node
    stake-used: uint,
    voted-at: uint
  }
)

(define-map node-penalties
  { node: principal }
  {
    total-penalties: uint,
    active-disputes: uint,
    reputation-score: uint
  }
)

;; Data variables
(define-data-var next-dispute-id uint u1)
(define-data-var total-arbitrators uint u0)

;; Read-only functions
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators { arbitrator: arbitrator })
)

(define-read-only (get-dispute-vote (dispute-id uint) (arbitrator principal))
  (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator })
)

(define-read-only (get-node-penalties (node principal))
  (default-to 
    { total-penalties: u0, active-disputes: u0, reputation-score: u100 }
    (map-get? node-penalties { node: node })
  )
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-read-only (is-dispute-active (dispute-id uint))
  (match (get-dispute dispute-id)
    dispute-data (and 
      (is-eq (get status dispute-data) DISPUTE_PENDING)
      (<= block-height (get voting-end dispute-data))
    )
    false
  )
)

;; Register as arbitrator
(define-public (register-arbitrator (stake-amount uint))
  (let (
    ;; Validate and sanitize input
    (validated-stake (if (>= stake-amount MIN_ARBITRATOR_STAKE) stake-amount u0))
    (max-allowed-stake u1000000000) ;; 1000 STX max
  )
    (asserts! (>= stake-amount MIN_ARBITRATOR_STAKE) ERR_INSUFFICIENT_STAKE)
    (asserts! (<= stake-amount max-allowed-stake) ERR_INSUFFICIENT_STAKE)
    
    ;; Check if already registered
    (asserts! (is-none (get-arbitrator tx-sender)) ERR_INVALID_DISPUTE)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? validated-stake tx-sender (as-contract tx-sender)))
    
    ;; Register arbitrator
    (map-set arbitrators
      { arbitrator: tx-sender }
      {
        stake: validated-stake,
        reputation: u100,
        active: true,
        total-votes: u0,
        correct-votes: u0
      }
    )
    
    (var-set total-arbitrators (+ (var-get total-arbitrators) u1))
    (ok true)
  )
)

;; Update arbitrator stake
(define-public (update-arbitrator-stake (additional-stake uint))
  (let ((current-arbitrator (unwrap! (get-arbitrator tx-sender) ERR_NOT_FOUND)))
    (asserts! (get active current-arbitrator) ERR_UNAUTHORIZED)
    
    ;; Transfer additional stake
    (try! (stx-transfer? additional-stake tx-sender (as-contract tx-sender)))
    
    ;; Update stake amount
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge current-arbitrator { stake: (+ (get stake current-arbitrator) additional-stake) })
    )
    
    (ok true)
  )
)

;; File a dispute
(define-public (file-dispute 
  (accused-node principal)
  (complaint-type uint)
  (file-hash (string-ascii 64))
  (description (string-ascii 500))
  (evidence-hash (string-ascii 64))
  (refund-amount uint))
  (let (
    (dispute-id (var-get next-dispute-id))
    (voting-end (+ block-height VOTING_PERIOD))
    ;; Validate and sanitize inputs
    (validated-complaint-type (if (<= complaint-type COMPLAINT_UNAUTHORIZED_ACCESS) complaint-type u0))
    (validated-refund (if (<= refund-amount u100000000) refund-amount u0)) ;; Max 100 STX refund
    (validated-file-hash (if (> (len file-hash) u0) file-hash ""))
    (validated-evidence-hash (if (> (len evidence-hash) u0) evidence-hash ""))
    (validated-description (if (> (len description) u0) description ""))
  )
    ;; Validate complaint type
    (asserts! (<= complaint-type COMPLAINT_UNAUTHORIZED_ACCESS) ERR_INVALID_DISPUTE)
    
    ;; Validate accused node is not the complainant
    (asserts! (not (is-eq accused-node tx-sender)) ERR_INVALID_DISPUTE)
    
    ;; Validate refund amount is reasonable
    (asserts! (<= refund-amount u100000000) ERR_INVALID_DISPUTE) ;; Max 100 STX
    
    ;; Validate string inputs are not empty
    (asserts! (> (len file-hash) u0) ERR_INVALID_DISPUTE)
    (asserts! (> (len evidence-hash) u0) ERR_INVALID_DISPUTE)
    (asserts! (> (len description) u10) ERR_INVALID_DISPUTE) ;; Min 10 chars description
    
    ;; Charge dispute fee
    (try! (stx-transfer? DISPUTE_FEE tx-sender (as-contract tx-sender)))
    
    ;; Create dispute record with validated inputs
    (map-set disputes
      { dispute-id: dispute-id }
      {
        complainant: tx-sender,
        accused-node: accused-node,
        complaint-type: validated-complaint-type,
        file-hash: validated-file-hash,
        description: validated-description,
        evidence-hash: validated-evidence-hash,
        status: DISPUTE_PENDING,
        created-at: block-height,
        voting-end: voting-end,
        votes-for-user: u0,
        votes-for-node: u0,
        total-stake-for-user: u0,
        total-stake-for-node: u0,
        refund-amount: validated-refund
      }
    )
    
    ;; Update node penalties (increment active disputes)
    (let ((current-penalties (get-node-penalties accused-node)))
      (map-set node-penalties
        { node: accused-node }
        (merge current-penalties { active-disputes: (+ (get active-disputes current-penalties) u1) })
      )
    )
    
    ;; Increment dispute ID for next dispute
    (var-set next-dispute-id (+ dispute-id u1))
    
    (ok dispute-id)
  )
)

;; Submit vote on dispute
(define-public (vote-on-dispute (dispute-id uint) (vote uint) (stake-to-use uint))
  (let (
    (dispute-data (unwrap! (get-dispute dispute-id) ERR_NOT_FOUND))
    (arbitrator-data (unwrap! (get-arbitrator tx-sender) ERR_UNAUTHORIZED))
    (existing-vote (get-dispute-vote dispute-id tx-sender))
    ;; Validate and sanitize inputs
    (validated-vote (if (or (is-eq vote u1) (is-eq vote u2)) vote u0))
    (validated-stake (if (<= stake-to-use (get stake arbitrator-data)) stake-to-use u0))
  )
    ;; Validate vote is in active period
    (asserts! (is-dispute-active dispute-id) ERR_VOTING_PERIOD_ENDED)
    
    ;; Validate arbitrator is active
    (asserts! (get active arbitrator-data) ERR_UNAUTHORIZED)
    
    ;; Validate vote value (1 = for user, 2 = for node)
    (asserts! (or (is-eq vote u1) (is-eq vote u2)) ERR_INVALID_VOTE)
    
    ;; Validate stake amount
    (asserts! (<= stake-to-use (get stake arbitrator-data)) ERR_INSUFFICIENT_STAKE)
    (asserts! (> stake-to-use u0) ERR_INSUFFICIENT_STAKE)
    
    ;; Check if already voted
    (asserts! (is-none existing-vote) ERR_INVALID_VOTE)
    
    ;; Validate arbitrator is not involved in the dispute
    (asserts! (not (is-eq tx-sender (get complainant dispute-data))) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq tx-sender (get accused-node dispute-data))) ERR_UNAUTHORIZED)
    
    ;; Record vote with validated inputs
    (map-set dispute-votes
      { dispute-id: dispute-id, arbitrator: tx-sender }
      {
        vote: validated-vote,
        stake-used: validated-stake,
        voted-at: block-height
      }
    )
    
    ;; Update dispute vote counts with validated inputs
    (if (is-eq validated-vote u1)
      ;; Vote for user
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          votes-for-user: (+ (get votes-for-user dispute-data) u1),
          total-stake-for-user: (+ (get total-stake-for-user dispute-data) validated-stake)
        })
      )
      ;; Vote for node
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          votes-for-node: (+ (get votes-for-node dispute-data) u1),
          total-stake-for-node: (+ (get total-stake-for-node dispute-data) validated-stake)
        })
      )
    )
    
    ;; Update arbitrator stats
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { total-votes: (+ (get total-votes arbitrator-data) u1) })
    )
    
    (ok true)
  )
)

;; Resolve dispute based on votes
(define-public (resolve-dispute (dispute-id uint))
  (let (
    (dispute-data (unwrap! (get-dispute dispute-id) ERR_NOT_FOUND))
  )
    ;; Check if dispute is still pending
    (asserts! (is-eq (get status dispute-data) DISPUTE_PENDING) ERR_DISPUTE_ALREADY_RESOLVED)
    
    ;; Check if voting period has ended
    (asserts! (> block-height (get voting-end dispute-data)) ERR_VOTING_PERIOD_ENDED)
    
    ;; Check if minimum quorum reached
    (asserts! (>= (+ (get votes-for-user dispute-data) (get votes-for-node dispute-data)) QUORUM_THRESHOLD) ERR_INVALID_DISPUTE)
    
    ;; Determine winner based on stake-weighted votes
    (let (
      (user-stake (get total-stake-for-user dispute-data))
      (node-stake (get total-stake-for-node dispute-data))
      (winner (if (> user-stake node-stake) DISPUTE_RESOLVED_FOR_USER DISPUTE_RESOLVED_FOR_NODE))
    )
      (begin
        ;; Update dispute status
        (map-set disputes
          { dispute-id: dispute-id }
          (merge dispute-data { status: winner })
        )
        
        ;; Process resolution
        (if (is-eq winner DISPUTE_RESOLVED_FOR_USER)
          ;; User wins - refund and penalize node
          (begin
            (try! (as-contract (stx-transfer? (get refund-amount dispute-data) tx-sender (get complainant dispute-data))))
            (unwrap-panic (penalize-node (get accused-node dispute-data)))
          )
          ;; Node wins - no refund, update node reputation positively
          (unwrap-panic (update-node-reputation (get accused-node dispute-data) true))
        )
        
        ;; Update arbitrator reputations
        (unwrap-panic (update-arbitrator-reputations dispute-id winner))
        
        ;; Decrease active disputes for node
        (map-set node-penalties
          { node: (get accused-node dispute-data) }
          (merge (get-node-penalties (get accused-node dispute-data)) 
                 { active-disputes: (- (get active-disputes (get-node-penalties (get accused-node dispute-data))) u1) })
        )
        
        (ok winner)
      )
    )
  )
)

;; Internal function to penalize dishonest node
(define-private (penalize-node (node principal))
  (let ((current-penalties (get-node-penalties node)))
    (map-set node-penalties
      { node: node }
      (merge current-penalties {
        total-penalties: (+ (get total-penalties current-penalties) u1),
        reputation-score: (if (> (get reputation-score current-penalties) u10) 
                           (- (get reputation-score current-penalties) u10) 
                           u0)
      })
    )
    (ok true)
  )
)

;; Internal function to update node reputation
(define-private (update-node-reputation (node principal) (positive bool))
  (let ((current-penalties (get-node-penalties node)))
    (map-set node-penalties
      { node: node }
      (merge current-penalties {
        reputation-score: (if positive
                           (if (< (get reputation-score current-penalties) u95)
                             (+ (get reputation-score current-penalties) u5)
                             u100)
                           (if (> (get reputation-score current-penalties) u5)
                             (- (get reputation-score current-penalties) u5)
                             u0))
      })
    )
    (ok true)
  )
)

;; Internal function to update arbitrator reputations based on correct votes
(define-private (update-arbitrator-reputations (dispute-id uint) (correct-outcome uint))
  ;; This is a simplified version - in a full implementation, you'd iterate through all voters
  ;; For now, we'll leave this as a placeholder that returns ok
  (ok true)
)

;; Cancel dispute (only by complainant before voting ends)
(define-public (cancel-dispute (dispute-id uint))
  (let (
    (dispute-data (unwrap! (get-dispute dispute-id) ERR_NOT_FOUND))
  )
    ;; Only complainant can cancel
    (asserts! (is-eq tx-sender (get complainant dispute-data)) ERR_UNAUTHORIZED)
    
    ;; Can only cancel pending disputes
    (asserts! (is-eq (get status dispute-data) DISPUTE_PENDING) ERR_DISPUTE_ALREADY_RESOLVED)
    
    ;; Update status
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data { status: DISPUTE_CANCELLED })
    )
    
    ;; Decrease active disputes for node
    (let ((current-penalties (get-node-penalties (get accused-node dispute-data))))
      (map-set node-penalties
        { node: (get accused-node dispute-data) }
        (merge current-penalties { active-disputes: (- (get active-disputes current-penalties) u1) })
      )
    )
    
    ;; Refund half the dispute fee
    (try! (as-contract (stx-transfer? (/ DISPUTE_FEE u2) tx-sender (get complainant dispute-data))))
    
    (ok true)
  )
)

;; Withdraw arbitrator stake (if not active in any ongoing disputes)
(define-public (withdraw-arbitrator-stake)
  (let (
    (arbitrator-data (unwrap! (get-arbitrator tx-sender) ERR_NOT_FOUND))
  )
    ;; Mark arbitrator as inactive
    (map-set arbitrators
      { arbitrator: tx-sender }
      (merge arbitrator-data { active: false })
    )
    
    ;; Transfer stake back (in a real implementation, you'd check for ongoing disputes)
    (try! (as-contract (stx-transfer? (get stake arbitrator-data) tx-sender tx-sender)))
    
    (var-set total-arbitrators (- (var-get total-arbitrators) u1))
    (ok true)
  )
)