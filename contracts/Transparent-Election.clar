(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-election-not-active (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-invalid-candidate (err u106))
(define-constant err-election-ended (err u107))
(define-constant err-election-not-ended (err u108))

(define-data-var election-counter uint u0)
(define-data-var current-election-id uint u0)

(define-map elections
  { election-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    start-block: uint,
    end-block: uint,
    is-active: bool,
    total-votes: uint,
    creator: principal
  }
)

(define-map candidates
  { election-id: uint, candidate-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    vote-count: uint
  }
)

(define-map candidate-counter
  { election-id: uint }
  { count: uint }
)

(define-map eligible-voters
  { election-id: uint, voter: principal }
  { is-eligible: bool }
)

(define-map voter-records
  { election-id: uint, voter: principal }
  {
    has-voted: bool,
    vote-block: uint,
    vote-hash: (buff 32)
  }
)

(define-map election-results
  { election-id: uint }
  {
    winner-candidate-id: uint,
    winner-vote-count: uint,
    is-finalized: bool
  }
)

(define-public (create-election (title (string-ascii 100)) (description (string-ascii 500)) (duration-blocks uint))
  (let
    (
      (new-election-id (+ (var-get election-counter) u1))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration-blocks))
    )
    (map-set elections
      { election-id: new-election-id }
      {
        title: title,
        description: description,
        start-block: start-block,
        end-block: end-block,
        is-active: true,
        total-votes: u0,
        creator: tx-sender
      }
    )
    (map-set candidate-counter
      { election-id: new-election-id }
      { count: u0 }
    )
    (var-set election-counter new-election-id)
    (var-set current-election-id new-election-id)
    (ok new-election-id)
  )
)

(define-public (add-candidate (election-id uint) (name (string-ascii 50)) (description (string-ascii 200)))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (counter-data (unwrap! (map-get? candidate-counter { election-id: election-id }) err-not-found))
      (new-candidate-id (+ (get count counter-data) u1))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (get is-active election) err-election-not-active)
    (asserts! (< stacks-block-height (get start-block election)) err-election-not-active)
    
    (map-set candidates
      { election-id: election-id, candidate-id: new-candidate-id }
      {
        name: name,
        description: description,
        vote-count: u0
      }
    )
    (map-set candidate-counter
      { election-id: election-id }
      { count: new-candidate-id }
    )
    (ok new-candidate-id)
  )
)

(define-public (register-voter (election-id uint) (voter principal))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (< stacks-block-height (get start-block election)) err-election-not-active)
    
    (map-set eligible-voters
      { election-id: election-id, voter: voter }
      { is-eligible: true }
    )
    (ok true)
  )
)

(define-public (register-multiple-voters (election-id uint) (voters (list 50 principal)))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (< stacks-block-height (get start-block election)) err-election-not-active)
    
    (ok (map register-single-voter voters))
  )
)

(define-private (register-single-voter (voter principal))
  (map-set eligible-voters
    { election-id: (var-get current-election-id), voter: voter }
    { is-eligible: true }
  )
)

(define-public (cast-vote (election-id uint) (candidate-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (voter-eligibility (unwrap! (map-get? eligible-voters { election-id: election-id, voter: tx-sender }) err-unauthorized))
      (existing-vote (map-get? voter-records { election-id: election-id, voter: tx-sender }))
      (candidate (unwrap! (map-get? candidates { election-id: election-id, candidate-id: candidate-id }) err-invalid-candidate))
      (vote-hash (sha256 (concat (unwrap-panic (to-consensus-buff? tx-sender)) (unwrap-panic (to-consensus-buff? candidate-id)))))
    )
    (asserts! (get is-eligible voter-eligibility) err-unauthorized)
    (asserts! (get is-active election) err-election-not-active)
    (asserts! (>= stacks-block-height (get start-block election)) err-election-not-active)
    (asserts! (< stacks-block-height (get end-block election)) err-election-ended)
    (asserts! (is-none existing-vote) err-already-voted)
    
    (map-set voter-records
      { election-id: election-id, voter: tx-sender }
      {
        has-voted: true,
        vote-block: stacks-block-height,
        vote-hash: vote-hash
      }
    )
    
    (map-set candidates
      { election-id: election-id, candidate-id: candidate-id }
      {
        name: (get name candidate),
        description: (get description candidate),
        vote-count: (+ (get vote-count candidate) u1)
      }
    )
    
    (map-set elections
      { election-id: election-id }
      {
        title: (get title election),
        description: (get description election),
        start-block: (get start-block election),
        end-block: (get end-block election),
        is-active: (get is-active election),
        total-votes: (+ (get total-votes election) u1),
        creator: (get creator election)
      }
    )
    
    (ok true)
  )
)

(define-public (finalize-election (election-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (counter-data (unwrap! (map-get? candidate-counter { election-id: election-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (>= stacks-block-height (get end-block election)) err-election-not-ended)
    
    (let
      (
        (winner-data (find-winner election-id (get count counter-data)))
      )
      (map-set elections
        { election-id: election-id }
        {
          title: (get title election),
          description: (get description election),
          start-block: (get start-block election),
          end-block: (get end-block election),
          is-active: false,
          total-votes: (get total-votes election),
          creator: (get creator election)
        }
      )
      
      (map-set election-results
        { election-id: election-id }
        {
          winner-candidate-id: (get candidate-id winner-data),
          winner-vote-count: (get vote-count winner-data),
          is-finalized: true
        }
      )
      
      (ok winner-data)
    )
  )
)

(define-private (find-winner (election-id uint) (total-candidates uint))
  (fold find-highest-votes 
    (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)
    { election-id: election-id, candidate-id: u1, vote-count: u0, max-candidates: total-candidates }
  )
)

(define-private (find-highest-votes (candidate-id uint) (acc { election-id: uint, candidate-id: uint, vote-count: uint, max-candidates: uint }))
  (if (> candidate-id (get max-candidates acc))
    acc
    (let
      (
        (candidate-data (map-get? candidates { election-id: (get election-id acc), candidate-id: candidate-id }))
      )
      (match candidate-data
        candidate
        (if (> (get vote-count candidate) (get vote-count acc))
          { election-id: (get election-id acc), candidate-id: candidate-id, vote-count: (get vote-count candidate), max-candidates: (get max-candidates acc) }
          acc
        )
        acc
      )
    )
  )
)

(define-read-only (get-election (election-id uint))
  (map-get? elections { election-id: election-id })
)

(define-read-only (get-candidate (election-id uint) (candidate-id uint))
  (map-get? candidates { election-id: election-id, candidate-id: candidate-id })
)

(define-read-only (get-voter-status (election-id uint) (voter principal))
  (map-get? voter-records { election-id: election-id, voter: voter })
)

(define-read-only (is-voter-eligible (election-id uint) (voter principal))
  (map-get? eligible-voters { election-id: election-id, voter: voter })
)

(define-read-only (get-election-results (election-id uint))
  (map-get? election-results { election-id: election-id })
)

(define-read-only (get-candidate-count (election-id uint))
  (map-get? candidate-counter { election-id: election-id })
)

(define-read-only (get-current-election-id)
  (var-get current-election-id)
)

(define-read-only (get-total-elections)
  (var-get election-counter)
)

(define-read-only (is-election-active (election-id uint))
  (match (map-get? elections { election-id: election-id })
    election
    (and 
      (get is-active election)
      (>= stacks-block-height (get start-block election))
      (< stacks-block-height (get end-block election))
    )
    false
  )
)
