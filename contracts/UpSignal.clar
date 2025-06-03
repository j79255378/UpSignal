(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_PROPOSAL_EXPIRED (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_INVALID_VOTE_WEIGHT (err u104))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u105))
(define-constant ERR_INSUFFICIENT_STAKE (err u106))

(define-data-var proposal-counter uint u0)
(define-data-var min-proposal-stake uint u1000000)
(define-data-var voting-period uint u1008)

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposer: principal,
    start-height: uint,
    end-height: uint,
    yes-votes: uint,
    no-votes: uint,
    total-stake: uint,
    status: (string-ascii 20),
    upgrade-hash: (buff 32)
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool,
    stake: uint,
    stacks-block-height: uint
  }
)

(define-map user-stakes
  { user: principal }
  { stake: uint }
)

(define-map validator-weights
  { validator: principal }
  { weight: uint }
)

(define-public (stake-tokens (amount uint))
  (let ((current-stake (default-to u0 (get stake (map-get? user-stakes { user: tx-sender })))))
    (map-set user-stakes
      { user: tx-sender }
      { stake: (+ current-stake amount) }
    )
    (ok amount)
  )
)

(define-public (unstake-tokens (amount uint))
  (let ((current-stake (default-to u0 (get stake (map-get? user-stakes { user: tx-sender })))))
    (asserts! (>= current-stake amount) ERR_INSUFFICIENT_STAKE)
    (map-set user-stakes
      { user: tx-sender }
      { stake: (- current-stake amount) }
    )
    (ok amount)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (upgrade-hash (buff 32)))
  (let (
    (user-stake (default-to u0 (get stake (map-get? user-stakes { user: tx-sender }))))
    (proposal-id (+ (var-get proposal-counter) u1))
    (current-height stacks-block-height)
  )
    (asserts! (>= user-stake (var-get min-proposal-stake)) ERR_INSUFFICIENT_STAKE)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        start-height: current-height,
        end-height: (+ current-height (var-get voting-period)),
        yes-votes: u0,
        no-votes: u0,
        total-stake: u0,
        status: "active",
        upgrade-hash: upgrade-hash
      }
    )
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    (user-stake (default-to u0 (get stake (map-get? user-stakes { user: tx-sender }))))
    (validator-weight (default-to u1 (get weight (map-get? validator-weights { validator: tx-sender }))))
    (effective-vote-weight (* user-stake validator-weight))
    (current-height stacks-block-height)
  )
    (asserts! (> effective-vote-weight u0) ERR_INVALID_VOTE_WEIGHT)
    (asserts! (is-eq (get status proposal) "active") ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (<= current-height (get end-height proposal)) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR_ALREADY_VOTED)
    
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: vote,
        stake: effective-vote-weight,
        stacks-block-height: current-height
      }
    )
    
    (let (
      (updated-proposal
        (merge proposal
          {
            yes-votes: (if vote (+ (get yes-votes proposal) effective-vote-weight) (get yes-votes proposal)),
            no-votes: (if vote (get no-votes proposal) (+ (get no-votes proposal) effective-vote-weight)),
            total-stake: (+ (get total-stake proposal) effective-vote-weight)
          }
        )
      )
    )
      (map-set proposals { proposal-id: proposal-id } updated-proposal)
      (ok effective-vote-weight)
    )
  )
)

(define-public (finalize-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    (current-height stacks-block-height)
  )
    (asserts! (> current-height (get end-height proposal)) ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (is-eq (get status proposal) "active") ERR_PROPOSAL_NOT_ACTIVE)
    
    (let (
      (yes-votes (get yes-votes proposal))
      (no-votes (get no-votes proposal))
      (total-votes (+ yes-votes no-votes))
      (approval-threshold (/ (* total-votes u67) u100))
      (final-status (if (>= yes-votes approval-threshold) "passed" "rejected"))
    )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: final-status })
      )
      (ok final-status)
    )
  )
)

(define-public (set-validator-weight (validator principal) (weight uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set validator-weights
      { validator: validator }
      { weight: weight }
    )
    (ok weight)
  )
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set voting-period new-period)
    (ok new-period)
  )
)

(define-public (update-min-stake (new-stake uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set min-proposal-stake new-stake)
    (ok new-stake)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-user-stake (user principal))
  (default-to u0 (get stake (map-get? user-stakes { user: user })))
)

(define-read-only (get-validator-weight (validator principal))
  (default-to u1 (get weight (map-get? validator-weights { validator: validator })))
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-min-proposal-stake)
  (var-get min-proposal-stake)
)

(define-read-only (calculate-consensus (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) (err "proposal not found")))
    (yes-votes (get yes-votes proposal))
    (no-votes (get no-votes proposal))
    (total-votes (+ yes-votes no-votes))
  )
    (if (> total-votes u0)
      (ok {
        yes-percentage: (/ (* yes-votes u100) total-votes),
        no-percentage: (/ (* no-votes u100) total-votes),
        total-stake: total-votes,
        consensus-reached: (>= yes-votes (/ (* total-votes u67) u100))
      })
      (ok {
        yes-percentage: u0,
        no-percentage: u0,
        total-stake: u0,
        consensus-reached: false
      })
    )
  )
)

;; (define-read-only (get-active-proposals)
;;   (let ((count (var-get proposal-counter)))
;;     (filter-active-proposals u1 count)
;;   )
;; )

;; (define-private (filter-active-proposals (start uint) (end uint))
;;   (if (<= start end)
;;     (let ((proposal (map-get? proposals { proposal-id: start })))
;;       (if (and (is-some proposal) (is-eq (get status (unwrap-panic proposal)) "active"))
;;         (append (list start) (filter-active-proposals (+ start u1) end))
;;         (filter-active-proposals (+ start u1) end)
;;       )
;;     )
;;     (list)
;;   )
;; )