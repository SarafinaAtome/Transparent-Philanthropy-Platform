;; Transparent Philanthropy Platform
;; Track donations and their usage for accountability

;; Data Maps
(define-map donations 
    { donation-id: uint }
    {
        donor: principal,
        amount: uint,
        cause: principal,
        status: (string-ascii 64)
    }
)

(define-map donor-leaderboard
    principal
    {
        total-donated: uint,
        donation-count: uint,
        last-donation: uint,
        engagement-score: uint,
        rank: uint
    }
)

(define-map leaderboard-rewards
    uint
    {
        reward-pool: uint,
        top-donors: (list 10 principal),
        distribution-date: uint,
        status: (string-ascii 20)
    }
)

(define-data-var reward-period-nonce uint u0)
(define-data-var current-leaderboard-size uint u0)

(define-map donation-counts principal uint)

;; Data Variables
(define-data-var donation-nonce uint u0)

;; Public Functions
(define-public (make-donation (amount uint) (cause principal))
    (let
        ((new-id (var-get donation-nonce)))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set donations
            { donation-id: new-id }
            {
                donor: tx-sender,
                amount: amount,
                cause: cause,
                status: "pending"
            }
        )
        (var-set donation-nonce (+ new-id u1))
        (map-set donation-counts tx-sender 
            (+ (default-to u0 (map-get? donation-counts tx-sender)) u1))
        (ok new-id)
    )
)

;; Read Only Functions
(define-read-only (get-donation (donation-id uint))
    (ok (map-get? donations {donation-id: donation-id}))
)

(define-read-only (get-donor-donation-count (donor principal))
    (ok (default-to u0 (map-get? donation-counts donor)))
)




(define-public (refund-donation (donation-id uint))
    (let (
        (donation (unwrap! (map-get? donations {donation-id: donation-id}) (err u1)))
        (donor (get donor donation))
        (amount (get amount donation))
    )
    (asserts! (is-eq (get status donation) "pending") (err u2))
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) donor)))
    (map-set donations 
        {donation-id: donation-id}
        (merge donation {status: "refunded"})
    )
    (ok true))
)



(define-public (approve-donation (donation-id uint))
    (let (
        (donation (unwrap! (map-get? donations {donation-id: donation-id}) (err u1)))
        (cause (get cause donation))
        (amount (get amount donation))
    )
    (asserts! (is-eq (get status donation) "pending") (err u2))
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) cause)))
    (map-set donations 
        {donation-id: donation-id}
        (merge donation {status: "approved"})
    )
    (ok true))
)



(define-public (reject-donation (donation-id uint))
    (let (
        (donation (unwrap! (map-get? donations {donation-id: donation-id}) (err u1)))
    )
    (asserts! (is-eq (get status donation) "pending") (err u2))
    (map-set donations 
        {donation-id: donation-id}
        (merge donation {status: "rejected"})
    )
    (ok true))
)




(define-map donor-reputation
    principal
    {score: uint, total-donated: uint}
)

(define-public (update-donor-reputation (donor principal) (amount uint))
    (let ((current-rep (default-to {score: u0, total-donated: u0} 
                        (map-get? donor-reputation donor))))
        (map-set donor-reputation donor
            {
                score: (+ (get score current-rep) u1),
                total-donated: (+ (get total-donated current-rep) amount)
            }
        )
        (ok true)
    )
)




(define-map matching-pools
    uint
    {matcher: principal, amount: uint, multiplier: uint}
)

(define-public (create-matching-pool (amount uint) (multiplier uint))
    (let ((pool-id (var-get donation-nonce)))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set matching-pools pool-id
            {matcher: tx-sender, amount: amount, multiplier: multiplier}
        )
        (ok pool-id)
    )
)


(define-public (match-donation (donation-id uint) (pool-id uint))
    (let (
        (donation (unwrap! (map-get? donations {donation-id: donation-id}) (err u1)))
        (pool (unwrap! (map-get? matching-pools pool-id) (err u2)))
        (donor (get donor donation))
        (amount (get amount donation))
        (matcher (get matcher pool))
        (multiplier (get multiplier pool))
        (matched-amount (* amount multiplier))
    )
    (asserts! (is-eq (get status donation) "pending") (err u3))
    (asserts! (is-eq matcher tx-sender) (err u4))
    (try! (as-contract (stx-transfer? matched-amount (as-contract tx-sender) donor)))
    (map-set donations 
        {donation-id: donation-id}
        (merge donation {status: "matched"})
    )
    (ok true)
))

(define-map categories 
    uint 
    {name: (string-ascii 64), description: (string-ascii 256)}
)

(define-data-var category-nonce uint u0)

(define-public (create-category (name (string-ascii 64)) (description (string-ascii 256)))
    (let ((cat-id (var-get category-nonce)))
        (map-set categories cat-id
            {name: name, description: description}
        )
        (var-set category-nonce (+ cat-id u1))
        (ok cat-id)
    )
)

(define-read-only (get-category (category-id uint))
    (ok (map-get? categories category-id))
)


;; Add at top with other data maps
(define-map donation-goals
    uint
    {target: uint, current: uint, title: (string-ascii 64), deadline: uint}
)

(define-data-var goal-nonce uint u0)

(define-public (create-goal (target uint) (title (string-ascii 64)) (deadline uint))
    (let ((goal-id (var-get goal-nonce)))
        (map-set donation-goals goal-id
            {target: target, current: u0, title: title, deadline: deadline}
        )
        (var-set goal-nonce (+ goal-id u1))
        (ok goal-id)
    )
)

(define-read-only (get-goal-progress (goal-id uint))
    (ok (map-get? donation-goals goal-id))
)


;; Add at top with other data maps
(define-map recurring-donations
    uint
    {donor: principal, amount: uint, cause: principal, interval: uint, last-donation: uint}
)

(define-data-var recurring-nonce uint u0)

(define-public (setup-recurring-donation (amount uint) (cause principal) (interval uint))
    (let ((donation-id (var-get recurring-nonce)))
        (map-set recurring-donations donation-id
            {
                donor: tx-sender,
                amount: amount,
                cause: cause,
                interval: interval,
                last-donation: stacks-block-height
            }
        )
        (var-set recurring-nonce (+ donation-id u1))
        (ok donation-id)
    )
)

(define-public (process-recurring-donation (donation-id uint))
    (let (
        (recurring (unwrap! (map-get? recurring-donations donation-id) (err u1)))
        (current-height stacks-block-height)
        (next-donation (+ (get last-donation recurring) (get interval recurring)))
    )
        (asserts! (>= current-height next-donation) (err u2))
        (try! (stx-transfer? (get amount recurring) tx-sender (get cause recurring)))
        (map-set recurring-donations donation-id
            (merge recurring {last-donation: current-height})
        )
        (ok true)
    )
)

;; Add at top with other data maps
(define-map donation-reports
    uint 
    {
        total-donations: uint,
        unique-donors: uint,
        largest-donation: uint,
        last-updated: uint
    }
)

(define-public (update-donation-report (report-id uint))
    (let (
        (current-report (default-to 
            {total-donations: u0, unique-donors: u0, largest-donation: u0, last-updated: u0}
            (map-get? donation-reports report-id)
        ))
    )
        (map-set donation-reports report-id
            (merge current-report 
                {
                    last-updated: stacks-block-height
                }
            )
        )
        (ok true)
    )
)

(define-read-only (get-donation-report (report-id uint))
    (ok (map-get? donation-reports report-id))
)



(define-map milestone-achievements
    uint 
    {
        threshold: uint,
        name: (string-ascii 64),
        token-uri: (string-utf8 256)
    }
)

(define-map donor-achievements
    { donor: principal, milestone-id: uint }
    { achieved: bool }
)

(define-data-var milestone-nonce uint u0)

(define-public (create-milestone (threshold uint) (name (string-ascii 64)) (token-uri (string-utf8 256)))
    (let ((milestone-id (var-get milestone-nonce)))
        (map-set milestone-achievements milestone-id
            {
                threshold: threshold,
                name: name,
                token-uri: token-uri
            }
        )
        (var-set milestone-nonce (+ milestone-id u1))
        (ok milestone-id)
    )
)


(define-map impact-metrics
    uint
    {
        donation-id: uint,
        metric-name: (string-ascii 64),
        value: uint,
        description: (string-utf8 256),
        timestamp: uint
    }
)

(define-data-var impact-nonce uint u0)

(define-public (record-impact (donation-id uint) (metric-name (string-ascii 64)) (value uint) (description (string-utf8 256)))
    (let (
        (impact-id (var-get impact-nonce))
        (donation (unwrap! (map-get? donations {donation-id: donation-id}) (err u1)))
    )
        (asserts! (is-eq (get cause donation) tx-sender) (err u2))
        (map-set impact-metrics impact-id
            {
                donation-id: donation-id,
                metric-name: metric-name,
                value: value,
                description: description,
                timestamp: stacks-block-height
            }
        )
        (var-set impact-nonce (+ impact-id u1))
        (ok impact-id)
    )
)

(define-read-only (get-donation-impact (donation-id uint))
    (ok (map-get? impact-metrics donation-id))
    )


(define-map donation-bundles
    uint 
    {
        donor: principal,
        total-amount: uint,
        allocations: (list 10 {cause: principal, percentage: uint})
    }
)

(define-data-var bundle-nonce uint u0)

(define-public (create-donation-bundle (total-amount uint) (allocations (list 10 {cause: principal, percentage: uint})))
    (let 
        ((bundle-id (var-get bundle-nonce))
         (total-percentage (fold + (map get-percentage allocations) u0)))
        
        (asserts! (is-eq total-percentage u100) (err u1))
        (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
        
        (map-set donation-bundles bundle-id
            {
                donor: tx-sender,
                total-amount: total-amount,
                allocations: allocations
            }
        )
        (var-set bundle-nonce (+ bundle-id u1))
        (ok bundle-id)
    )
)

(define-private (get-percentage (allocation {cause: principal, percentage: uint}))
    (get percentage allocation)
)

(define-public (execute-bundle (bundle-id uint))
    (let 
        ((bundle (unwrap! (map-get? donation-bundles bundle-id) (err u1))))
        (try! (process-allocations (get allocations bundle) (get total-amount bundle)) )
        (ok true)
    )
)

(define-private (process-allocations (allocations (list 10 {cause: principal, percentage: uint})) (total-amount uint))
    (begin 
        (fold process-single-allocation allocations (ok total-amount))
    )
)

(define-private (process-single-allocation (allocation {cause: principal, percentage: uint}) (result (response uint uint)))
    (match result 
        success (let 
            ((amount (/ (* (get percentage allocation) success) u100)))
            (try! (as-contract (stx-transfer? amount tx-sender (get cause allocation))))
            (ok success)
        )
        error (err error)
    )
)


(define-map verifiers principal bool)

(define-map donation-verifications
    uint
    {
        donation-id: uint,
        verifier: principal,
        status: (string-ascii 20),
        verification-date: uint,
        notes: (string-utf8 256)
    }
)

(define-data-var verification-nonce uint u0)

(define-public (register-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender tx-sender) (err u1))
        (map-set verifiers verifier true)
        (ok true)
    )
)

(define-public (verify-donation (donation-id uint) (status (string-ascii 20)) (notes (string-utf8 256)))
    (let 
        ((verification-id (var-get verification-nonce)))
        (asserts! (unwrap! (map-get? verifiers tx-sender) (err u1)) (err u2))
        
        (map-set donation-verifications verification-id
            {
                donation-id: donation-id,
                verifier: tx-sender,
                status: status,
                verification-date: stacks-block-height,
                notes: notes
            }
        )
        (var-set verification-nonce (+ verification-id u1))
        (ok verification-id)
    )
)

(define-read-only (get-donation-verification (verification-id uint))
    (ok (map-get? donation-verifications verification-id))
)


(define-map escrow-agreements
    uint
    {
        donation-id: uint,
        arbiter: principal,
        conditions: (string-utf8 512),
        deadline: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)

(define-map escrow-votes
    {agreement-id: uint, voter: principal}
    {vote: bool, timestamp: uint}
)

(define-data-var escrow-nonce uint u0)

(define-public (create-escrow-donation (amount uint) (cause principal) (arbiter principal) (conditions (string-utf8 512)) (deadline uint))
    (let 
        ((donation-id (var-get donation-nonce))
         (escrow-id (var-get escrow-nonce)))
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set donations
            {donation-id: donation-id}
            {
                donor: tx-sender,
                amount: amount,
                cause: cause,
                status: "escrowed"
            }
        )
        
        (map-set escrow-agreements escrow-id
            {
                donation-id: donation-id,
                arbiter: arbiter,
                conditions: conditions,
                deadline: deadline,
                status: "active",
                created-at: stacks-block-height
            }
        )
        
        (var-set donation-nonce (+ donation-id u1))
        (var-set escrow-nonce (+ escrow-id u1))
        (ok escrow-id)
    )
)

(define-public (vote-escrow-release (agreement-id uint) (approve bool))
    (let 
        ((agreement (unwrap! (map-get? escrow-agreements agreement-id) (err u1)))
         (donation (unwrap! (map-get? donations {donation-id: (get donation-id agreement)}) (err u2))))
        
        (asserts! (is-eq (get status agreement) "active") (err u3))
        (asserts! (or (is-eq tx-sender (get donor donation)) 
                     (is-eq tx-sender (get arbiter agreement))) (err u4))
        
        (map-set escrow-votes 
            {agreement-id: agreement-id, voter: tx-sender}
            {vote: approve, timestamp: stacks-block-height}
        )
        (ok true)
    )
)

(define-public (execute-escrow-release (agreement-id uint))
    (let 
        ((agreement (unwrap! (map-get? escrow-agreements agreement-id) (err u1)))
         (donation (unwrap! (map-get? donations {donation-id: (get donation-id agreement)}) (err u2)))
         (donor-vote (map-get? escrow-votes {agreement-id: agreement-id, voter: (get donor donation)}))
         (arbiter-vote (map-get? escrow-votes {agreement-id: agreement-id, voter: (get arbiter agreement)})))
        
        (asserts! (is-eq (get status agreement) "active") (err u3))
        (asserts! (< stacks-block-height (get deadline agreement)) (err u4))
        
        (asserts! (and 
            (is-some donor-vote)
            (is-some arbiter-vote)
            (get vote (unwrap-panic donor-vote))
            (get vote (unwrap-panic arbiter-vote))) (err u5))
        
        (try! (as-contract (stx-transfer? (get amount donation) (as-contract tx-sender) (get cause donation))))
        
        (map-set escrow-agreements agreement-id
            (merge agreement {status: "released"}))
        
        (map-set donations 
            {donation-id: (get donation-id agreement)}
            (merge donation {status: "released"}))
        
        (ok true)
    )
)

(define-public (cancel-escrow (agreement-id uint))
    (let 
        ((agreement (unwrap! (map-get? escrow-agreements agreement-id) (err u1)))
         (donation (unwrap! (map-get? donations {donation-id: (get donation-id agreement)}) (err u2))))
        
        (asserts! (is-eq (get status agreement) "active") (err u3))
        (asserts! (or (>= stacks-block-height (get deadline agreement))
                     (is-eq tx-sender (get donor donation))) (err u4))
        
        (try! (as-contract (stx-transfer? (get amount donation) (as-contract tx-sender) (get donor donation))))
        
        (map-set escrow-agreements agreement-id
            (merge agreement {status: "cancelled"}))
        
        (map-set donations 
            {donation-id: (get donation-id agreement)}
            (merge donation {status: "refunded"}))
        
        (ok true)
    )
)

(define-read-only (get-escrow-agreement (agreement-id uint))
    (ok (map-get? escrow-agreements agreement-id))
)

(define-read-only (get-escrow-vote (agreement-id uint) (voter principal))
    (ok (map-get? escrow-votes {agreement-id: agreement-id, voter: voter}))
)




(define-public (update-leaderboard-entry (donor principal) (amount uint))
    (let 
        ((current-entry (default-to 
            {total-donated: u0, donation-count: u0, last-donation: u0, engagement-score: u0, rank: u0}
            (map-get? donor-leaderboard donor)))
         (new-total (+ (get total-donated current-entry) amount))
         (new-count (+ (get donation-count current-entry) u1))
         (recency-bonus (if (< (- stacks-block-height (get last-donation current-entry)) u144) u10 u0))
         (new-score (+ (* new-total u2) (* new-count u5) recency-bonus)))
        
        (map-set donor-leaderboard donor
            {
                total-donated: new-total,
                donation-count: new-count,
                last-donation: stacks-block-height,
                engagement-score: new-score,
                rank: u0
            }
        )
        (ok true)
    )
)

(define-public (create-reward-pool (pool-amount uint))
    (let 
        ((period-id (var-get reward-period-nonce)))
        (try! (stx-transfer? pool-amount tx-sender (as-contract tx-sender)))
        (map-set leaderboard-rewards period-id
            {
                reward-pool: pool-amount,
                top-donors: (list),
                distribution-date: (+ stacks-block-height u1008),
                status: "active"
            }
        )
        (var-set reward-period-nonce (+ period-id u1))
        (ok period-id)
    )
)

(define-public (finalize-leaderboard-period (period-id uint) (top-donors (list 10 principal)))
    (let 
        ((reward-info (unwrap! (map-get? leaderboard-rewards period-id) (err u1))))
        (asserts! (is-eq (get status reward-info) "active") (err u2))
        (asserts! (>= stacks-block-height (get distribution-date reward-info)) (err u3))
        
        (map-set leaderboard-rewards period-id
            (merge reward-info 
                {
                    top-donors: top-donors,
                    status: "finalized"
                }
            )
        )
        (ok true)
    )
)

(define-public (claim-leaderboard-reward (period-id uint) (position uint))
    (let 
        ((reward-info (unwrap! (map-get? leaderboard-rewards period-id) (err u1)))
         (top-donors (get top-donors reward-info))
         (total-pool (get reward-pool reward-info))
         (reward-amount (get-reward-by-position position total-pool)))
        
        (asserts! (is-eq (get status reward-info) "finalized") (err u2))
        (asserts! (< position (len top-donors)) (err u3))
        (asserts! (is-eq tx-sender (unwrap! (element-at top-donors position) (err u4))) (err u5))
        
        (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) tx-sender)))
        (ok reward-amount)
    )
)

(define-private (get-reward-by-position (position uint) (total-pool uint))
    (if (is-eq position u0)
        (/ (* total-pool u50) u100)
        (if (is-eq position u1)
            (/ (* total-pool u30) u100)
            (if (is-eq position u2)
                (/ (* total-pool u20) u100)
                (/ total-pool u10)
            )
        )
    )
)

(define-read-only (get-donor-leaderboard-entry (donor principal))
    (ok (map-get? donor-leaderboard donor))
)

(define-read-only (get-reward-period-info (period-id uint))
    (ok (map-get? leaderboard-rewards period-id))
)

(define-read-only (calculate-engagement-score (donor principal))
    (let 
        ((entry (unwrap! (map-get? donor-leaderboard donor) (err u1)))
         (total-donated (get total-donated entry))
         (donation-count (get donation-count entry))
         (last-donation (get last-donation entry))
         (recency-bonus (if (< (- stacks-block-height last-donation) u144) u10 u0)))
        (ok (+ (* total-donated u2) (* donation-count u5) recency-bonus))
    )
)