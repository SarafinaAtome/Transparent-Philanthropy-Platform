;; Transparent Philanthropy Platform
;; Track donations and their usage for accountability

;; Data Maps
(define-map donations 
    { donation-id: uint }
    {
        donor: principal,
        amount: uint,
        cause: (string-ascii 256),
        status: (string-ascii 64)
    }
)

(define-map donation-counts principal uint)

;; Data Variables
(define-data-var donation-nonce uint u0)

;; Public Functions
(define-public (make-donation (amount uint) (cause (string-ascii 256)))
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
