;; Donor Feedback & Charity Reputation System
;; Enables donors to rate charities and provide detailed feedback on fund usage

;; Data Maps for feedback system
(define-map charity-feedback
    {charity: principal, donor: principal}
    {
        rating: uint,
        feedback-text: (string-utf8 512),
        transparency-score: uint,
        effectiveness-score: uint,
        communication-score: uint,
        timestamp: uint,
        donation-id: uint
    }
)

;; Aggregate charity reputation scores
(define-map charity-reputation
    principal
    {
        total-ratings: uint,
        average-rating: uint,
        transparency-avg: uint,
        effectiveness-avg: uint,
        communication-avg: uint,
        total-feedback-count: uint,
        reputation-level: (string-ascii 20),
        last-updated: uint
    }
)

;; Verified reviewer status for enhanced credibility
(define-map verified-reviewers
    principal
    {
        verified: bool,
        review-count: uint,
        credibility-score: uint,
        verification-date: uint
    }
)

;; Feedback response system for charities
(define-map charity-responses
    {charity: principal, feedback-id: uint}
    {
        response-text: (string-utf8 512),
        action-taken: (string-utf8 256),
        follow-up-commitment: (string-utf8 256),
        timestamp: uint
    }
)

;; Feedback helpfulness voting
(define-map feedback-votes
    {feedback-id: uint, voter: principal}
    {
        helpful: bool,
        timestamp: uint
    }
)

;; Feedback moderation flags
(define-map feedback-flags
    {feedback-id: uint, flagger: principal}
    {
        reason: (string-ascii 64),
        timestamp: uint,
        resolved: bool
    }
)

;; Data variables for system management
(define-data-var feedback-nonce uint u0)
(define-data-var response-nonce uint u0)
(define-data-var flag-nonce uint u0)
(define-data-var min-donation-threshold uint u1000000) ;; Minimum donation to leave feedback

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-INVALID-RATING (err u1002))
(define-constant ERR-ALREADY-REVIEWED (err u1003))
(define-constant ERR-DONATION-TOO-SMALL (err u1004))
(define-constant ERR-INVALID-DONATION (err u1005))
(define-constant ERR-FEEDBACK-NOT-FOUND (err u1006))
(define-constant ERR-ALREADY-VOTED (err u1007))
(define-constant ERR-SELF-VOTE (err u1008))
(define-constant ERR-INVALID-CHARITY (err u1009))

;; Main function to submit feedback about a charity
(define-public (submit-charity-feedback 
    (charity principal) 
    (donation-id uint)
    (rating uint) 
    (feedback-text (string-utf8 512))
    (transparency-score uint)
    (effectiveness-score uint)
    (communication-score uint))
    (let 
        ((feedback-id (var-get feedback-nonce))
         (donation-check (contract-call? .tpp get-donation donation-id)))
        
        ;; Validate input parameters
        (asserts! (and (<= rating u5) (>= rating u1)) ERR-INVALID-RATING)
        (asserts! (and (<= transparency-score u10) (>= transparency-score u1)) ERR-INVALID-RATING)
        (asserts! (and (<= effectiveness-score u10) (>= effectiveness-score u1)) ERR-INVALID-RATING)
        (asserts! (and (<= communication-score u10) (>= communication-score u1)) ERR-INVALID-RATING)
        
        ;; Check if donor hasn't already reviewed this charity
        (asserts! (is-none (map-get? charity-feedback {charity: charity, donor: tx-sender})) ERR-ALREADY-REVIEWED)
        
        ;; Verify donation exists and meets minimum threshold
        (let ((donation-result (unwrap! donation-check ERR-INVALID-DONATION)))
            (let ((donation-data (unwrap! donation-result ERR-INVALID-DONATION)))
                (asserts! (is-eq (get donor donation-data) tx-sender) ERR-NOT-AUTHORIZED)
                (asserts! (is-eq (get cause donation-data) charity) ERR-INVALID-CHARITY)
                (asserts! (>= (get amount donation-data) (var-get min-donation-threshold)) ERR-DONATION-TOO-SMALL)
                
                ;; Store the feedback
                (map-set charity-feedback 
                    {charity: charity, donor: tx-sender}
                    {
                        rating: rating,
                        feedback-text: feedback-text,
                        transparency-score: transparency-score,
                        effectiveness-score: effectiveness-score,
                        communication-score: communication-score,
                        timestamp: stacks-block-height,
                        donation-id: donation-id
                    }
                )
                
                ;; Update charity reputation scores
                (unwrap! (update-charity-reputation charity rating transparency-score effectiveness-score communication-score) ERR-INVALID-RATING)
                
                ;; Update verified reviewer stats if applicable
                (unwrap! (update-reviewer-credibility tx-sender) ERR-INVALID-RATING)
                
                (var-set feedback-nonce (+ feedback-id u1))
                (ok feedback-id)
            )
        )
    )
)

;; Update charity reputation based on new feedback
(define-private (update-charity-reputation 
    (charity principal) 
    (new-rating uint) 
    (transparency uint) 
    (effectiveness uint) 
    (communication uint))
    (let 
        ((current-rep (default-to 
            {total-ratings: u0, average-rating: u0, transparency-avg: u0, 
             effectiveness-avg: u0, communication-avg: u0, total-feedback-count: u0, 
             reputation-level: "unrated", last-updated: u0}
            (map-get? charity-reputation charity)))
         (new-count (+ (get total-feedback-count current-rep) u1))
         (prev-total-rating (* (get average-rating current-rep) (get total-feedback-count current-rep)))
         (new-avg-rating (/ (+ prev-total-rating new-rating) new-count))
         (prev-total-transparency (* (get transparency-avg current-rep) (get total-feedback-count current-rep)))
         (new-avg-transparency (/ (+ prev-total-transparency transparency) new-count))
         (prev-total-effectiveness (* (get effectiveness-avg current-rep) (get total-feedback-count current-rep)))
         (new-avg-effectiveness (/ (+ prev-total-effectiveness effectiveness) new-count))
         (prev-total-communication (* (get communication-avg current-rep) (get total-feedback-count current-rep)))
         (new-avg-communication (/ (+ prev-total-communication communication) new-count))
         (reputation-level (calculate-reputation-level new-avg-rating new-count)))
        
        (map-set charity-reputation charity
            {
                total-ratings: (+ (get total-ratings current-rep) new-rating),
                average-rating: new-avg-rating,
                transparency-avg: new-avg-transparency,
                effectiveness-avg: new-avg-effectiveness,
                communication-avg: new-avg-communication,
                total-feedback-count: new-count,
                reputation-level: reputation-level,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Calculate reputation level based on average rating and review count
(define-private (calculate-reputation-level (avg-rating uint) (review-count uint))
    (if (< review-count u3)
        "unrated"
        (if (>= avg-rating u45) ;; 4.5 and above
            "excellent"
            (if (>= avg-rating u40) ;; 4.0 and above
                "very-good"
                (if (>= avg-rating u35) ;; 3.5 and above
                    "good"
                    (if (>= avg-rating u25) ;; 2.5 and above
                        "fair"
                        "poor"
                    )
                )
            )
        )
    )
)

;; Update reviewer credibility based on activity
(define-private (update-reviewer-credibility (reviewer principal))
    (let 
        ((current-cred (default-to 
            {verified: false, review-count: u0, credibility-score: u0, verification-date: u0}
            (map-get? verified-reviewers reviewer)))
         (new-count (+ (get review-count current-cred) u1))
         (new-score (+ (get credibility-score current-cred) u1)))
        
        (map-set verified-reviewers reviewer
            (merge current-cred 
                {
                    review-count: new-count,
                    credibility-score: new-score
                }
            )
        )
        (ok true)
    )
)

;; Allow charities to respond to feedback
(define-public (respond-to-feedback 
    (feedback-donor principal)
    (response-text (string-utf8 512))
    (action-taken (string-utf8 256))
    (follow-up-commitment (string-utf8 256)))
    (let 
        ((response-id (var-get response-nonce))
         (feedback-exists (map-get? charity-feedback {charity: tx-sender, donor: feedback-donor})))
        
        (asserts! (is-some feedback-exists) ERR-FEEDBACK-NOT-FOUND)
        
        (map-set charity-responses 
            {charity: tx-sender, feedback-id: response-id}
            {
                response-text: response-text,
                action-taken: action-taken,
                follow-up-commitment: follow-up-commitment,
                timestamp: stacks-block-height
            }
        )
        
        (var-set response-nonce (+ response-id u1))
        (ok response-id)
    )
)

;; Vote on feedback helpfulness
(define-public (vote-feedback-helpfulness (charity principal) (donor principal) (helpful bool))
    (let 
        ((feedback-exists (map-get? charity-feedback {charity: charity, donor: donor})))
        
        (asserts! (is-some feedback-exists) ERR-FEEDBACK-NOT-FOUND)
        (asserts! (not (is-eq tx-sender donor)) ERR-SELF-VOTE)
        (asserts! (is-none (map-get? feedback-votes {feedback-id: u0, voter: tx-sender})) ERR-ALREADY-VOTED)
        
        (map-set feedback-votes 
            {feedback-id: u0, voter: tx-sender}
            {
                helpful: helpful,
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Flag inappropriate feedback for moderation
(define-public (flag-feedback (charity principal) (donor principal) (reason (string-ascii 64)))
    (let 
        ((flag-id (var-get flag-nonce))
         (feedback-exists (map-get? charity-feedback {charity: charity, donor: donor})))
        
        (asserts! (is-some feedback-exists) ERR-FEEDBACK-NOT-FOUND)
        
        (map-set feedback-flags 
            {feedback-id: flag-id, flagger: tx-sender}
            {
                reason: reason,
                timestamp: stacks-block-height,
                resolved: false
            }
        )
        
        (var-set flag-nonce (+ flag-id u1))
        (ok flag-id)
    )
)

;; Admin function to verify reviewers manually
(define-public (verify-reviewer (reviewer principal))
    (let 
        ((current-status (default-to 
            {verified: false, review-count: u0, credibility-score: u0, verification-date: u0}
            (map-get? verified-reviewers reviewer))))
        
        (map-set verified-reviewers reviewer
            (merge current-status 
                {
                    verified: true,
                    verification-date: stacks-block-height
                }
            )
        )
        (ok true)
    )
)

;; Read-only functions for data retrieval
(define-read-only (get-charity-feedback (charity principal) (donor principal))
    (ok (map-get? charity-feedback {charity: charity, donor: donor}))
)

(define-read-only (get-charity-reputation (charity principal))
    (ok (map-get? charity-reputation charity))
)

(define-read-only (get-reviewer-status (reviewer principal))
    (ok (map-get? verified-reviewers reviewer))
)

(define-read-only (get-charity-response (charity principal) (feedback-id uint))
    (ok (map-get? charity-responses {charity: charity, feedback-id: feedback-id}))
)

(define-read-only (get-feedback-vote (feedback-id uint) (voter principal))
    (ok (map-get? feedback-votes {feedback-id: feedback-id, voter: voter}))
)

(define-read-only (get-feedback-flag (flag-id uint) (flagger principal))
    (ok (map-get? feedback-flags {feedback-id: flag-id, flagger: flagger}))
)

;; Get charity performance metrics
(define-read-only (get-charity-performance-summary (charity principal))
    (let 
        ((reputation (map-get? charity-reputation charity)))
        (match reputation
            rep-data (ok {
                overall-rating: (get average-rating rep-data),
                transparency: (get transparency-avg rep-data),
                effectiveness: (get effectiveness-avg rep-data),
                communication: (get communication-avg rep-data),
                total-reviews: (get total-feedback-count rep-data),
                reputation-level: (get reputation-level rep-data)
            })
            (ok {
                overall-rating: u0,
                transparency: u0,
                effectiveness: u0,
                communication: u0,
                total-reviews: u0,
                reputation-level: "unrated"
            })
        )
    )
)

;; Update minimum donation threshold for feedback eligibility
(define-public (update-min-donation-threshold (new-threshold uint))
    (begin
        (var-set min-donation-threshold new-threshold)
        (ok true)
    )
)

(define-read-only (get-min-donation-threshold)
    (ok (var-get min-donation-threshold))
)

