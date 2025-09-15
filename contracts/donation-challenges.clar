;; Donation Challenge System
;; Enables community-driven donation challenges with competitive elements

;; Challenge data structure
(define-map donation-challenges
    uint
    {
        creator: principal,
        title: (string-ascii 64),
        description: (string-utf8 256),
        target-amount: uint,
        current-amount: uint,
        cause: principal,
        start-height: uint,
        end-height: uint,
        status: (string-ascii 20),
        participant-count: uint,
        reward-pool: uint,
        challenge-type: (string-ascii 20)
    }
)

;; Track individual participants in challenges
(define-map challenge-participants
    {challenge-id: uint, participant: principal}
    {
        contribution: uint,
        joined-at: uint,
        rank: uint,
        eligible-for-rewards: bool
    }
)

;; Challenge leaderboard for top contributors
(define-map challenge-leaderboard
    {challenge-id: uint, rank: uint}
    {
        participant: principal,
        contribution: uint,
        percentage-of-target: uint
    }
)

;; Challenge completion milestones
(define-map challenge-milestones
    {challenge-id: uint, milestone: uint}
    {
        percentage: uint,
        bonus-reward: uint,
        achieved: bool,
        achieved-at: uint
    }
)

;; Data variables
(define-data-var challenge-nonce uint u0)
(define-data-var milestone-nonce uint u0)
(define-data-var min-challenge-duration uint u144) ;; ~1 day in blocks
(define-data-var max-challenge-duration uint u4032) ;; ~4 weeks in blocks
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u2001))
(define-constant ERR-INVALID-CHALLENGE (err u2002))
(define-constant ERR-CHALLENGE-ENDED (err u2003))
(define-constant ERR-CHALLENGE-NOT-STARTED (err u2004))
(define-constant ERR-INSUFFICIENT-AMOUNT (err u2005))
(define-constant ERR-ALREADY-PARTICIPATED (err u2006))
(define-constant ERR-INVALID-DURATION (err u2007))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u2008))
(define-constant ERR-MILESTONE-NOT-FOUND (err u2009))

;; Create a new donation challenge
(define-public (create-challenge 
    (title (string-ascii 64))
    (description (string-utf8 256))
    (target-amount uint)
    (cause principal)
    (duration uint)
    (reward-pool uint)
    (challenge-type (string-ascii 20)))
    (let 
        ((challenge-id (var-get challenge-nonce))
         (start-height stacks-block-height)
         (end-height (+ start-height duration)))
        
        ;; Validate duration
        (asserts! (and (>= duration (var-get min-challenge-duration))
                      (<= duration (var-get max-challenge-duration))) ERR-INVALID-DURATION)
        (asserts! (> target-amount u0) ERR-INSUFFICIENT-AMOUNT)
        
        ;; Transfer reward pool to contract
        (try! (stx-transfer? reward-pool tx-sender (as-contract tx-sender)))
        
        ;; Store challenge data
        (map-set donation-challenges challenge-id
            {
                creator: tx-sender,
                title: title,
                description: description,
                target-amount: target-amount,
                current-amount: u0,
                cause: cause,
                start-height: start-height,
                end-height: end-height,
                status: "active",
                participant-count: u0,
                reward-pool: reward-pool,
                challenge-type: challenge-type
            }
        )
        
        ;; Create default milestones at 25%, 50%, 75%, 100%
        (unwrap! (create-milestone challenge-id u25 (/ reward-pool u8)) ERR-INVALID-CHALLENGE)
        (unwrap! (create-milestone challenge-id u50 (/ reward-pool u6)) ERR-INVALID-CHALLENGE)
        (unwrap! (create-milestone challenge-id u75 (/ reward-pool u4)) ERR-INVALID-CHALLENGE)
        (unwrap! (create-milestone challenge-id u100 (/ reward-pool u3)) ERR-INVALID-CHALLENGE)
        
        (var-set challenge-nonce (+ challenge-id u1))
        (ok challenge-id)
    )
)

;; Participate in a challenge by making a donation
(define-public (participate-in-challenge (challenge-id uint) (donation-amount uint))
    (let 
        ((challenge (unwrap! (map-get? donation-challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
         (existing-participation (map-get? challenge-participants {challenge-id: challenge-id, participant: tx-sender})))
        
        ;; Validate challenge is active and within time bounds
        (asserts! (is-eq (get status challenge) "active") ERR-INVALID-CHALLENGE)
        (asserts! (>= stacks-block-height (get start-height challenge)) ERR-CHALLENGE-NOT-STARTED)
        (asserts! (< stacks-block-height (get end-height challenge)) ERR-CHALLENGE-ENDED)
        (asserts! (> donation-amount u0) ERR-INSUFFICIENT-AMOUNT)
        
        ;; Transfer donation to cause (through the main contract)
        (try! (contract-call? .tpp make-donation donation-amount (get cause challenge)))
        
        ;; Update or create participant record
        (match existing-participation
            existing-data
                (map-set challenge-participants 
                    {challenge-id: challenge-id, participant: tx-sender}
                    (merge existing-data 
                        {
                            contribution: (+ (get contribution existing-data) donation-amount),
                            eligible-for-rewards: true
                        }
                    )
                )
            ;; New participant
            (begin
                (map-set challenge-participants 
                    {challenge-id: challenge-id, participant: tx-sender}
                    {
                        contribution: donation-amount,
                        joined-at: stacks-block-height,
                        rank: u0,
                        eligible-for-rewards: true
                    }
                )
                ;; Increment participant count for new participants only
                (map-set donation-challenges challenge-id
                    (merge challenge 
                        {
                            participant-count: (+ (get participant-count challenge) u1)
                        }
                    )
                )
            )
        )
        
        ;; Update challenge total
        (map-set donation-challenges challenge-id
            (merge challenge 
                {
                    current-amount: (+ (get current-amount challenge) donation-amount)
                }
            )
        )
        
        ;; Check for milestone achievements
        (unwrap! (check-milestone-achievements challenge-id) ERR-INVALID-CHALLENGE)
        
        (ok donation-amount)
    )
)

;; Create challenge milestone
(define-private (create-milestone (challenge-id uint) (percentage uint) (bonus-reward uint))
    (let ((milestone-id (var-get milestone-nonce)))
        (map-set challenge-milestones 
            {challenge-id: challenge-id, milestone: percentage}
            {
                percentage: percentage,
                bonus-reward: bonus-reward,
                achieved: false,
                achieved-at: u0
            }
        )
        (var-set milestone-nonce (+ milestone-id u1))
        (ok milestone-id)
    )
)

;; Check and process milestone achievements
(define-private (check-milestone-achievements (challenge-id uint))
    (let 
        ((challenge (unwrap! (map-get? donation-challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
         (current-percentage (/ (* (get current-amount challenge) u100) (get target-amount challenge))))
        
        ;; Check each milestone
        (unwrap! (process-milestone challenge-id u25 current-percentage) ERR-INVALID-CHALLENGE)
        (unwrap! (process-milestone challenge-id u50 current-percentage) ERR-INVALID-CHALLENGE)
        (unwrap! (process-milestone challenge-id u75 current-percentage) ERR-INVALID-CHALLENGE)
        (unwrap! (process-milestone challenge-id u100 current-percentage) ERR-INVALID-CHALLENGE)
        (ok true)
    )
)

;; Process individual milestone
(define-private (process-milestone (challenge-id uint) (milestone-percent uint) (current-percent uint))
    (let ((milestone (map-get? challenge-milestones {challenge-id: challenge-id, milestone: milestone-percent})))
        (match milestone
            milestone-data
                (if (and (>= current-percent milestone-percent) (not (get achieved milestone-data)))
                    (begin
                        (map-set challenge-milestones 
                            {challenge-id: challenge-id, milestone: milestone-percent}
                            (merge milestone-data 
                                {
                                    achieved: true,
                                    achieved-at: stacks-block-height
                                }
                            )
                        )
                        (ok true)
                    )
                    (ok true)
                )
            (ok true)
        )
    )
)

;; End a challenge and finalize results
(define-public (finalize-challenge (challenge-id uint))
    (let 
        ((challenge (unwrap! (map-get? donation-challenges challenge-id) ERR-CHALLENGE-NOT-FOUND)))
        
        ;; Only creator can finalize, and challenge must be ended
        (asserts! (is-eq tx-sender (get creator challenge)) ERR-UNAUTHORIZED)
        (asserts! (>= stacks-block-height (get end-height challenge)) ERR-CHALLENGE-ENDED)
        (asserts! (is-eq (get status challenge) "active") ERR-INVALID-CHALLENGE)
        
        ;; Update challenge status
        (map-set donation-challenges challenge-id
            (merge challenge {status: "completed"})
        )
        
        (ok true)
    )
)

;; Claim milestone reward
(define-public (claim-milestone-reward (challenge-id uint) (milestone-percent uint))
    (let 
        ((challenge (unwrap! (map-get? donation-challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
         (milestone (unwrap! (map-get? challenge-milestones {challenge-id: challenge-id, milestone: milestone-percent}) ERR-MILESTONE-NOT-FOUND))
         (participation (unwrap! (map-get? challenge-participants {challenge-id: challenge-id, participant: tx-sender}) ERR-UNAUTHORIZED)))
        
        ;; Validate conditions
        (asserts! (get achieved milestone) ERR-MILESTONE-NOT-FOUND)
        (asserts! (get eligible-for-rewards participation) ERR-UNAUTHORIZED)
        (asserts! (> (get contribution participation) u0) ERR-INSUFFICIENT-AMOUNT)
        
        ;; Calculate reward based on contribution percentage
        (let 
            ((contribution-percentage (/ (* (get contribution participation) u100) (get current-amount challenge)))
             (reward-amount (/ (* (get bonus-reward milestone) contribution-percentage) u100)))
            
            ;; Transfer reward to participant
            (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) tx-sender)))
            
            ;; Mark as claimed by setting eligibility to false for this milestone
            (map-set challenge-participants 
                {challenge-id: challenge-id, participant: tx-sender}
                (merge participation {eligible-for-rewards: false})
            )
            
            (ok reward-amount)
        )
    )
)

;; Read-only functions
(define-read-only (get-challenge (challenge-id uint))
    (ok (map-get? donation-challenges challenge-id))
)

(define-read-only (get-challenge-participation (challenge-id uint) (participant principal))
    (ok (map-get? challenge-participants {challenge-id: challenge-id, participant: participant}))
)

(define-read-only (get-challenge-milestone (challenge-id uint) (milestone-percent uint))
    (ok (map-get? challenge-milestones {challenge-id: challenge-id, milestone: milestone-percent}))
)

(define-read-only (get-challenge-progress (challenge-id uint))
    (let ((challenge (map-get? donation-challenges challenge-id)))
        (match challenge
            challenge-data (ok {
                target: (get target-amount challenge-data),
                current: (get current-amount challenge-data),
                percentage: (/ (* (get current-amount challenge-data) u100) (get target-amount challenge-data)),
                participants: (get participant-count challenge-data),
                time-remaining: (if (> (get end-height challenge-data) stacks-block-height)
                                   (- (get end-height challenge-data) stacks-block-height)
                                   u0)
            })
            (err ERR-CHALLENGE-NOT-FOUND)
        )
    )
)

(define-read-only (is-challenge-active (challenge-id uint))
    (let ((challenge (map-get? donation-challenges challenge-id)))
        (match challenge
            challenge-data (ok (and 
                (is-eq (get status challenge-data) "active")
                (>= stacks-block-height (get start-height challenge-data))
                (< stacks-block-height (get end-height challenge-data))
            ))
            (ok false)
        )
    )
)

;; Get active challenges (simple version - returns first 10)
(define-read-only (get-active-challenges)
    (ok {
        count: (var-get challenge-nonce),
        note: "Use get-challenge to retrieve individual challenge details"
    })
)
