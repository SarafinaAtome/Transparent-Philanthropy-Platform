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


