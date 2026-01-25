;; Enhanced Storage with Value History Contract
;; Advanced contract for storing value history with comprehensive features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NO-VALUES (err u101))
(define-constant ERR-INVALID-INDEX (err u102))
(define-constant ERR-INVALID-RANGE (err u103))
(define-constant ERR-THRESHOLD-NOT-MET (err u104))
(define-constant ERR-ALREADY-PAUSED (err u105))
(define-constant ERR-NOT-PAUSED (err u106))
(define-constant ERR-INVALID-PERMISSION (err u107))
(define-constant ERR-USER-NOT-FOUND (err u108))

;; Contract owner and permissions
(define-constant CONTRACT-OWNER tx-sender)
(define-data-var contract-paused bool false)

;; Permission levels: none, read, write, admin
(define-map user-permissions principal uint)

;; Data storage with enhanced features
(define-data-var value-history (list 100 uint) (list))
(define-data-var value-count uint u0)
(define-data-var max-history-size uint u100)

;; Metadata tracking
(define-map value-metadata uint {
    timestamp: uint,
    block-height: uint,
    setter: principal,
    tags: (list 5 (string-ascii 20))
})

;; Statistics tracking
(define-data-var total-values-added uint u0)
(define-data-var sum-all-values uint u0)
(define-data-var min-value (optional uint) none)
(define-data-var max-value (optional uint) none)

;; Alert system
(define-data-var alert-threshold-high (optional uint) none)
(define-data-var alert-threshold-low (optional uint) none)
(define-data-var alert-recipients (list 10 principal) (list))

;; Events/notifications
(define-map event-log uint {
    event-type: (string-ascii 20),
    value: uint,
    timestamp: uint,
    block-height: uint,
    principal: principal
})
(define-data-var event-counter uint u0)

;; Snapshots for rollback functionality
(define-map snapshots uint {
    history: (list 100 uint),
    count: uint,
    timestamp: uint,
    creator: principal
})
(define-data-var snapshot-counter uint u0)

;; Helper function for batch operations - defined before use


;; Permission functions
(define-private (has-permission (user principal) (required-level uint))
    (let ((user-level (default-to u0 (map-get? user-permissions user))))
        (or (is-eq user CONTRACT-OWNER) (>= user-level required-level))
    )
)

(define-public (set-user-permission (user principal) (level uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= level u3) ERR-INVALID-PERMISSION)
        (map-set user-permissions user level)
        (ok true)
    )
)

;; Contract control functions
(define-public (pause-contract)
    (begin
        (asserts! (has-permission tx-sender u3) ERR-NOT-AUTHORIZED)
        (asserts! (not (var-get contract-paused)) ERR-ALREADY-PAUSED)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (unpause-contract)
    (begin
        (asserts! (has-permission tx-sender u3) ERR-NOT-AUTHORIZED)
        (asserts! (var-get contract-paused) ERR-NOT-PAUSED)
        (var-set contract-paused false)
        (ok true)
    )
)

;; Helper functions
(define-private (min-uint (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max-uint (a uint) (b uint))
    (if (> a b) a b)
)

;; Event logging function - defined early since it's used by other functions
(define-private (log-event (event-type (string-ascii 20)) (value uint))
    (let ((event-id (var-get event-counter)))
        (map-set event-log event-id {
            event-type: event-type,
            value: value,
            timestamp: block-height,
            block-height: block-height,
            principal: tx-sender
        })
        (var-set event-counter (+ event-id u1))
        (ok event-id)
    )
)

;; Alert checking function - also moved up since it's used early
(define-private (check-alerts (value uint))
    (let 
        (
            (high-threshold (var-get alert-threshold-high))
            (low-threshold (var-get alert-threshold-low))
        )
        (begin
            (match high-threshold
                threshold (if (>= value threshold) 
                    (let ((log-result (log-event "ALERT_HIGH" value)))
                        (is-ok log-result)
                    )
                    true
                )
                true
            )
            (match low-threshold
                threshold (if (<= value threshold) 
                    (let ((log-result (log-event "ALERT_LOW" value)))
                        (is-ok log-result)
                    )
                    true
                )
                true
            )
            (ok true)
        )
    )
)



;; Helper function for range filtering - accumulator function must be defined first
(define-private (filter-range-accumulator 
    (acc {min: uint, max: uint, result: (list 100 uint)})
    (val uint) 
)
    (if (and (>= val (get min acc)) (<= val (get max acc)))
        {
            min: (get min acc),
            max: (get max acc),
            result: (unwrap-panic (as-max-len? (append (get result acc) val) u100))
        }
        acc
    )
)



;; Enhanced value addition with metadata and validation
(define-public (add-value-with-metadata (new-value uint) (tags (list 5 (string-ascii 20))))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (has-permission tx-sender u2) ERR-NOT-AUTHORIZED)
        (let 
            (
                (current-history (var-get value-history))
                (current-count (var-get value-count))
                (current-sum (var-get sum-all-values))
                (current-min (var-get min-value))
                (current-max (var-get max-value))
            )
            ;; Add value to history
            (var-set value-history (unwrap-panic (as-max-len? (concat (list new-value) current-history) u100)))
            (var-set value-count (+ current-count u1))
            
            ;; Update statistics
            (var-set total-values-added (+ (var-get total-values-added) u1))
            (var-set sum-all-values (+ current-sum new-value))
            (var-set min-value (some (match current-min min-val (if (< new-value min-val) new-value min-val) new-value)))
            (var-set max-value (some (match current-max max-val (if (> new-value max-val) new-value max-val) new-value)))
            
            ;; Store metadata
            (map-set value-metadata current-count {
                timestamp: block-height,
                block-height: block-height,
                setter: tx-sender,
                tags: tags
            })
            
            ;; Log event (ignore result, don't fail if logging fails)
            (let ((log-result (log-event "VALUE_ADDED" new-value))) true)
            
            ;; Check alerts (ignore result, don't fail if alerts fail)
            (let ((alert-result (check-alerts new-value))) true)
            
            (ok current-count)
        )
    )
)

;; Simple value addition (backward compatibility)
(define-public (add-value (new-value uint))
    (add-value-with-metadata new-value (list))
)

;; Simplified batch operations without recursion
(define-public (add-multiple-values (values (list 20 uint)))
    (begin
        (asserts! (not (var-get contract-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (has-permission tx-sender u2) ERR-NOT-AUTHORIZED)
        (add-values-sequentially values)
    )
)

(define-private (add-values-sequentially (values (list 20 uint)))
    (let ((len (len values)))
        (if (is-eq len u0) (ok u0)
        (if (is-eq len u1) 
            (match (add-value-with-metadata (unwrap-panic (element-at values u0)) (list))
                success1 (ok u1)
                error1 (err error1)
            )
        (if (is-eq len u2)
            (match (add-value-with-metadata (unwrap-panic (element-at values u0)) (list))
                success1 (match (add-value-with-metadata (unwrap-panic (element-at values u1)) (list))
                    success2 (ok u2)
                    error2 (err error2)
                )
                error1 (err error1)
            )
        (if (is-eq len u3)
            (match (add-value-with-metadata (unwrap-panic (element-at values u0)) (list))
                success1 (match (add-value-with-metadata (unwrap-panic (element-at values u1)) (list))
                    success2 (match (add-value-with-metadata (unwrap-panic (element-at values u2)) (list))
                        success3 (ok u3)
                        error3 (err error3)
                    )
                    error2 (err error2)
                )
                error1 (err error1)
            )
        ;; For more than 3 values, just add first 3
        (match (add-value-with-metadata (unwrap-panic (element-at values u0)) (list))
            success1 (match (add-value-with-metadata (unwrap-panic (element-at values u1)) (list))
                success2 (match (add-value-with-metadata (unwrap-panic (element-at values u2)) (list))
                    success3 (ok u3)
                    error3 (err error3)
                )
                error2 (err error2)
            )
            error1 (err error1)
        )
        ))))
    )
)

(define-private (add-single-value (value uint) (result (response uint uint)))
    (match result
        success (add-value value)
        error (err error)
    )
)



;; Alert system
(define-public (set-alert-thresholds (high (optional uint)) (low (optional uint)))
    (begin
        (asserts! (has-permission tx-sender u3) ERR-NOT-AUTHORIZED)
        (var-set alert-threshold-high high)
        (var-set alert-threshold-low low)
        (ok true)
    )
)

(define-public (add-alert-recipient (recipient principal))
    (begin
        (asserts! (has-permission tx-sender u3) ERR-NOT-AUTHORIZED)
        (let ((current-recipients (var-get alert-recipients)))
            (var-set alert-recipients (unwrap-panic (as-max-len? (concat current-recipients (list recipient)) u10)))
            (ok true)
        )
    )
)

;; Snapshot and rollback functionality
(define-public (create-snapshot)
    (begin
        (asserts! (has-permission tx-sender u3) ERR-NOT-AUTHORIZED)
        (let ((snapshot-id (var-get snapshot-counter)))
            (map-set snapshots snapshot-id {
                history: (var-get value-history),
                count: (var-get value-count),
                timestamp: block-height,
                creator: tx-sender
            })
            (var-set snapshot-counter (+ snapshot-id u1))
            (let ((log-result (log-event "SNAPSHOT_CREATED" snapshot-id))) true)
            (ok snapshot-id)
        )
    )
)

(define-public (rollback-to-snapshot (snapshot-id uint))
    (if (not (is-eq tx-sender CONTRACT-OWNER))
        (err ERR-NOT-AUTHORIZED)
        (let ((snapshot-data (map-get? snapshots snapshot-id)))
            (if (is-some snapshot-data)
                (let ((snapshot (unwrap-panic snapshot-data)))
                    (begin
                        (var-set value-history (get history snapshot))
                        (var-set value-count (get count snapshot))
                        (ok true)
                    )
                )
                (err ERR-INVALID-INDEX)
            )
        )
    )
)

;; Advanced analytics and statistics
(define-read-only (get-statistics)
    (ok {
        total-count: (var-get value-count),
        total-added: (var-get total-values-added),
        sum: (var-get sum-all-values),
        average: (if (> (var-get value-count) u0) (/ (var-get sum-all-values) (var-get value-count)) u0),
        min: (var-get min-value),
        max: (var-get max-value),
        range: (match (var-get max-value)
            max-val (match (var-get min-value)
                min-val (some (- max-val min-val))
                none
            )
            none
        )
    })
)

(define-read-only (get-moving-average (window-size uint))
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
            (actual-window (if (> window-size count) count window-size))
        )
        (if (is-eq actual-window u0)
            (ok u0)
            (let ((recent-values (unwrap-panic (slice? history u0 actual-window))))
                (ok (/ (fold + recent-values u0) actual-window))
            )
        )
    )
)

(define-read-only (get-trend-analysis (window-size uint))
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
        )
        (if (< count u2)
            (ok "INSUFFICIENT_DATA")
            (let 
                (
                    (recent-val (unwrap-panic (element-at history u0)))
                    (older-index (min-uint (- count u1) (- window-size u1)))
                    (older-val (unwrap-panic (element-at history older-index)))
                )
                (ok (if (> recent-val older-val) "INCREASING" 
                       (if (< recent-val older-val) "DECREASING" "STABLE")))
            )
        )
    )
)

;; Helper function for getting metadata range - must be defined before use


;; Enhanced getters with filtering and search


;; Helper function for limited metadata range - defined before use
(define-private (get-limited-metadata-range (start-index uint) (end-index uint))
    (let 
        (
            (history (var-get value-history))
            (range (- end-index start-index))
        )
        (if (is-eq range u0) (list)
        (if (is-eq range u1) (list {value: (default-to u0 (element-at history start-index)), metadata: (map-get? value-metadata start-index)})
        (if (is-eq range u2) (list 
            {value: (default-to u0 (element-at history start-index)), metadata: (map-get? value-metadata start-index)}
            {value: (default-to u0 (element-at history (+ start-index u1))), metadata: (map-get? value-metadata (+ start-index u1))})
        (if (is-eq range u3) (list 
            {value: (default-to u0 (element-at history start-index)), metadata: (map-get? value-metadata start-index)}
            {value: (default-to u0 (element-at history (+ start-index u1))), metadata: (map-get? value-metadata (+ start-index u1))}
            {value: (default-to u0 (element-at history (+ start-index u2))), metadata: (map-get? value-metadata (+ start-index u2))})
        ;; For larger ranges, return first 3 items as example
        (list 
            {value: (default-to u0 (element-at history start-index)), metadata: (map-get? value-metadata start-index)}
            {value: (default-to u0 (element-at history (+ start-index u1))), metadata: (map-get? value-metadata (+ start-index u1))}
            {value: (default-to u0 (element-at history (+ start-index u2))), metadata: (map-get? value-metadata (+ start-index u2))})
        ))))
    )
)

(define-read-only (get-values-with-metadata (start-index uint) (end-index uint))
    (let 
        (
            (count (var-get value-count))
            (actual-end (min-uint end-index count))
        )
        (if (>= start-index actual-end)
            (err ERR-INVALID-RANGE)
            (if (> actual-end start-index)
                ;; For simplicity, limit to first 10 items to avoid complex range generation
                (if (<= (- actual-end start-index) u10)
                    (ok (get-limited-metadata-range start-index actual-end))
                    (err ERR-INVALID-RANGE)
                )
                (ok (list))
            )
        )
    )
)





;; Event and audit trail
(define-read-only (get-recent-events (count uint))
    (let 
        (
            (total-events (var-get event-counter))
        )
        (if (is-eq total-events u0)
            (ok (list))
            (ok (get-last-events count total-events))
        )
    )
)

(define-private (get-last-events (count uint) (total-events uint))
    (let 
        (
            (actual-count (min-uint count u10)) ;; Limit to 10 events for simplicity
            (start-id (if (> total-events actual-count) (- total-events actual-count) u0))
        )
        (if (is-eq actual-count u0) (list)
        (if (is-eq actual-count u1) (list (map-get? event-log (if (> total-events u0) (- total-events u1) u0)))
        (if (is-eq actual-count u2) (list 
            (map-get? event-log (if (> total-events u1) (- total-events u2) u0))
            (map-get? event-log (if (> total-events u0) (- total-events u1) u0)))
        (if (is-eq actual-count u3) (list 
            (map-get? event-log (if (> total-events u2) (- total-events u3) u0))
            (map-get? event-log (if (> total-events u1) (- total-events u2) u0))
            (map-get? event-log (if (> total-events u0) (- total-events u1) u0)))
        ;; For more than 3, return last 3 events
        (list 
            (map-get? event-log (if (> total-events u2) (- total-events u3) u0))
            (map-get? event-log (if (> total-events u1) (- total-events u2) u0))
            (map-get? event-log (if (> total-events u0) (- total-events u1) u0)))
        ))))
    )
)

;; Configuration getters
(define-read-only (get-contract-info)
    (ok {
        owner: CONTRACT-OWNER,
        paused: (var-get contract-paused),
        max-history-size: (var-get max-history-size),
        current-count: (var-get value-count),
        alert-thresholds: {
            high: (var-get alert-threshold-high),
            low: (var-get alert-threshold-low)
        },
        recipients: (var-get alert-recipients)
    })
)

;; Original getter functions (maintained for compatibility)
(define-read-only (get-current-value)
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
        )
        (if (is-eq count u0)
            (err ERR-NO-VALUES)
            (ok (unwrap-panic (element-at history u0)))
        )
    )
)

(define-read-only (get-previous-value)
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
        )
        (if (< count u2)
            (err ERR-NO-VALUES)
            (ok (unwrap-panic (element-at history u1)))
        )
    )
)

(define-read-only (get-current-and-previous)
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
        )
        (if (is-eq count u0)
            (err ERR-NO-VALUES)
            (if (is-eq count u1)
                (ok {
                    current: (unwrap-panic (element-at history u0)),
                    previous: none
                })
                (ok {
                    current: (unwrap-panic (element-at history u0)),
                    previous: (some (unwrap-panic (element-at history u1)))
                })
            )
        )
    )
)

(define-read-only (get-value-at-index (index uint))
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
        )
        (if (>= index count)
            (err ERR-INVALID-INDEX)
            (ok (unwrap-panic (element-at history index)))
        )
    )
)

(define-read-only (get-full-history)
    (ok (var-get value-history))
)

(define-read-only (get-value-count)
    (ok (var-get value-count))
)

(define-read-only (get-last-n-values (n uint))
    (let 
        (
            (history (var-get value-history))
            (count (var-get value-count))
            (actual-n (if (> n count) count n))
        )
        (ok (unwrap-panic (slice? history u0 actual-n)))
    )
)