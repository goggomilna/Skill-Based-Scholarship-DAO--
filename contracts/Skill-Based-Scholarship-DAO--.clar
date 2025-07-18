(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-EXISTS (err u102))
(define-constant ERR-NO-PROPOSAL (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-QUIZ-EXISTS (err u106))
(define-constant ERR-INVALID-QUIZ (err u107))

(define-fungible-token governance-token)

(define-data-var dao-owner principal tx-sender)
(define-data-var proposal-count uint u0)
(define-data-var quiz-count uint u0)
(define-data-var min-proposal-amount uint u100)
(define-data-var voting-period uint u144)

(define-map proposals
    uint 
    {
        id: uint,
        applicant: principal,
        amount: uint,
        description: (string-ascii 256),
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
        deadline: uint,
        tokens-locked: uint
    }
)

(define-map quizzes
    uint 
    {
        id: uint,
        reward: uint,
        hash: (buff 32),
        active: bool
    }
)

(define-map user-quiz-completion
    { user: principal, quiz-id: uint }
    bool
)

(define-map votes
    { proposal-id: uint, voter: principal }
    bool
)

(define-public (initialize (token-name (string-ascii 32)))
    (begin
        (try! (ft-mint? governance-token u1000000000 tx-sender))
        (ok true)))

(define-public (create-quiz (reward uint) (answer-hash (buff 32)))
    (let ((quiz-id (+ (var-get quiz-count) u1)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (map-set quizzes quiz-id
            {
                id: quiz-id,
                reward: reward,
                hash: answer-hash,
                active: true
            }
        )
        (var-set quiz-count quiz-id)
        (ok quiz-id)))

(define-public (submit-quiz-answer (quiz-id uint) (answer (buff 32)))
    (let (
        (quiz (unwrap! (map-get? quizzes quiz-id) ERR-INVALID-QUIZ))
        (completion-key { user: tx-sender, quiz-id: quiz-id })
    )
        (asserts! (is-eq (hash160 answer) (get hash quiz)) ERR-INVALID-QUIZ)
        (asserts! (not (default-to false (map-get? user-quiz-completion completion-key))) ERR-QUIZ-EXISTS)
        (try! (ft-mint? governance-token (get reward quiz) tx-sender))
        (map-set user-quiz-completion completion-key true)
        (ok true)))

(define-public (submit-proposal (amount uint) (description (string-ascii 256)))
    (let (
        (proposal-id (+ (var-get proposal-count) u1))
        (deadline (+ stacks-block-height (var-get voting-period)))
    )
        (asserts! (>= amount (var-get min-proposal-amount)) ERR-INVALID-AMOUNT)
        (try! (ft-transfer? governance-token amount tx-sender (as-contract tx-sender)))
        (map-set proposals proposal-id
            {
                id: proposal-id,
                applicant: tx-sender,
                amount: amount,
                description: description,
                yes-votes: u0,
                no-votes: u0,
                executed: false,
                deadline: deadline,
                tokens-locked: amount
            }
        )
        (var-set proposal-count proposal-id)
        (ok proposal-id)))
(define-public (vote (proposal-id uint) (vote-for bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-PROPOSAL))
        (vote-key { proposal-id: proposal-id, voter: tx-sender })
        (voter-balance (ft-get-balance governance-token tx-sender))
    )
        (asserts! (< stacks-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (not (default-to false (map-get? votes vote-key))) ERR-ALREADY-VOTED)
        (map-set proposals proposal-id
            (merge proposal
                {
                    yes-votes: (if vote-for (+ (get yes-votes proposal) voter-balance) (get yes-votes proposal)),
                    no-votes: (if vote-for (get no-votes proposal) (+ (get no-votes proposal) voter-balance))
                }
            )
        )
        (map-set votes vote-key true)
        (ok true)))

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-PROPOSAL))
        (current-height stacks-block-height)
    )
        (asserts! (>= current-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXISTS)
        (if (> (get yes-votes proposal) (get no-votes proposal))
            (begin
                (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get applicant proposal))))
                (map-set proposals proposal-id (merge proposal { executed: true }))
                (ok true)
            )
            (begin
                (try! (as-contract (ft-transfer? governance-token (get tokens-locked proposal) tx-sender (get applicant proposal))))
                (map-set proposals proposal-id (merge proposal { executed: true }))
                (ok false)
            ))))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-quiz (quiz-id uint))
    (map-get? quizzes quiz-id))

(define-read-only (get-user-vote (proposal-id uint) (user principal))
    (map-get? votes { proposal-id: proposal-id, voter: user }))
