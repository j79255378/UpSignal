(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u101))
(define-constant ERR_PROPOSAL_EXPIRED (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_INVALID_VOTE_WEIGHT (err u104))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u105))
(define-constant ERR_INSUFFICIENT_STAKE (err u106))
(define-constant ERR_SELF_DELEGATION (err u107))
(define-constant ERR_DELEGATION_NOT_FOUND (err u108))
(define-constant ERR_CIRCULAR_DELEGATION (err u109))
(define-constant ERR_INVALID_DELEGATION_AMOUNT (err u110))
(define-constant ERR_DELEGATION_LIMIT_EXCEEDED (err u111))

(define-data-var proposal-counter uint u0)
(define-data-var min-proposal-stake uint u1000000)
(define-data-var voting-period uint u1008)
(define-data-var max-delegation-depth uint u5)

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

(define-map delegations
  { delegator: principal }
  {
    delegate: principal,
    amount: uint,
    active: bool,
    delegation-height: uint
  }
)

(define-map delegation-received
  { delegate: principal }
  {
    total-delegated: uint,
    delegation-count: uint
  }
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

(define-private (check-simple-circular (delegate principal) (original-delegator principal))
  (match (map-get? delegations { delegator: delegate })
    next-delegation
    (if (get active next-delegation)
      (is-eq (get delegate next-delegation) original-delegator)
      false
    )
    false
  )
)

(define-read-only (check-circular-delegation (delegate principal) (original-delegator principal))
  (ok (check-simple-circular delegate original-delegator))
)

(define-public (delegate-voting-power (delegate principal) (amount uint))
  (let (
    (delegator-stake (default-to u0 (get stake (map-get? user-stakes { user: tx-sender }))))
    (existing-delegation (map-get? delegations { delegator: tx-sender }))
    (current-height stacks-block-height)
  )
    (asserts! (not (is-eq tx-sender delegate)) ERR_SELF_DELEGATION)
    (asserts! (> amount u0) ERR_INVALID_DELEGATION_AMOUNT)
    (asserts! (>= delegator-stake amount) ERR_INSUFFICIENT_STAKE)
    (asserts! (not (check-simple-circular delegate tx-sender)) ERR_CIRCULAR_DELEGATION)
    
    (match existing-delegation
      existing-del
      (let (
        (current-delegate (get delegate existing-del))
        (current-amount (get amount existing-del))
        (delegate-info (default-to { total-delegated: u0, delegation-count: u0 } 
                         (map-get? delegation-received { delegate: current-delegate })))
      )
        (map-set delegation-received 
          { delegate: current-delegate }
          { 
            total-delegated: (- (get total-delegated delegate-info) current-amount),
            delegation-count: (- (get delegation-count delegate-info) u1)
          }
        )
        (update-delegation-for-new-delegate delegate amount current-height)
      )
      (update-delegation-for-new-delegate delegate amount current-height)
    )
  )
)

(define-private (update-delegation-for-new-delegate (delegate principal) (amount uint) (current-height uint))
  (let (
    (new-delegate-info (default-to { total-delegated: u0, delegation-count: u0 } 
                         (map-get? delegation-received { delegate: delegate })))
  )
    (map-set delegations
      { delegator: tx-sender }
      {
        delegate: delegate,
        amount: amount,
        active: true,
        delegation-height: current-height
      }
    )
    (map-set delegation-received
      { delegate: delegate }
      {
        total-delegated: (+ (get total-delegated new-delegate-info) amount),
        delegation-count: (+ (get delegation-count new-delegate-info) u1)
      }
    )
    (ok amount)
  )
)

(define-public (revoke-delegation)
  (let (
    (existing-delegation (unwrap! (map-get? delegations { delegator: tx-sender }) ERR_DELEGATION_NOT_FOUND))
    (delegate (get delegate existing-delegation))
    (amount (get amount existing-delegation))
    (delegate-info (default-to { total-delegated: u0, delegation-count: u0 } 
                     (map-get? delegation-received { delegate: delegate })))
  )
    (map-set delegations
      { delegator: tx-sender }
      (merge existing-delegation { active: false })
    )
    (map-set delegation-received
      { delegate: delegate }
      {
        total-delegated: (- (get total-delegated delegate-info) amount),
        delegation-count: (- (get delegation-count delegate-info) u1)
      }
    )
    (ok amount)
  )
)

(define-public (vote-with-delegation (proposal-id uint) (vote bool) (delegator principal))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    (delegation (unwrap! (map-get? delegations { delegator: delegator }) ERR_DELEGATION_NOT_FOUND))
    (delegate (get delegate delegation))
    (delegated-amount (get amount delegation))
    (delegation-active (get active delegation))
    (current-height stacks-block-height)
    (delegate-stake (default-to u0 (get stake (map-get? user-stakes { user: tx-sender }))))
    (validator-weight (default-to u1 (get weight (map-get? validator-weights { validator: tx-sender }))))
  )
    (asserts! (is-eq tx-sender delegate) ERR_UNAUTHORIZED)
    (asserts! delegation-active ERR_DELEGATION_NOT_FOUND)
    (asserts! (> delegated-amount u0) ERR_INVALID_VOTE_WEIGHT)
    (asserts! (is-eq (get status proposal) "active") ERR_PROPOSAL_NOT_ACTIVE)
    (asserts! (<= current-height (get end-height proposal)) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: delegator })) ERR_ALREADY_VOTED)
    
    (let (
      (effective-vote-weight (* delegated-amount validator-weight))
    )
      (map-set votes
        { proposal-id: proposal-id, voter: delegator }
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
)

(define-public (get-effective-voting-power (user principal))
  (let (
    (user-stake (default-to u0 (get stake (map-get? user-stakes { user: user }))))
    (delegation (map-get? delegations { delegator: user }))
    (delegated-to-user (default-to { total-delegated: u0, delegation-count: u0 } 
                         (map-get? delegation-received { delegate: user })))
    (validator-weight (default-to u1 (get weight (map-get? validator-weights { validator: user }))))
  )
    (let (
      (available-stake (match delegation
        del (if (get active del) (- user-stake (get amount del)) user-stake)
        user-stake
      ))
      (received-delegation (get total-delegated delegated-to-user))
      (total-power (+ available-stake received-delegation))
    )
      (ok {
        own-stake: user-stake,
        available-stake: available-stake,
        delegated-out: (match delegation
          del (if (get active del) (get amount del) u0)
          u0
        ),
        delegated-in: received-delegation,
        total-voting-power: (* total-power validator-weight),
        delegation-count: (get delegation-count delegated-to-user)
      })
    )
  )
)



(define-read-only (get-delegation (delegator principal))
  (map-get? delegations { delegator: delegator })
)

(define-read-only (get-delegation-received (delegate principal))
  (map-get? delegation-received { delegate: delegate })
)

(define-read-only (get-max-delegation-depth)
  (var-get max-delegation-depth)
)

(define-public (update-max-delegation-depth (new-depth uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set max-delegation-depth new-depth)
    (ok new-depth)
  )
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