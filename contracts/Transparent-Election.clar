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
(define-constant err-invalid-delegation (err u109))
(define-constant err-delegation-not-found (err u110))
(define-constant err-cannot-delegate-to-self (err u111))
(define-constant err-audit-not-enabled (err u112))
(define-constant err-verification-failed (err u113))
(define-constant err-audit-already-started (err u114))

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

(define-map voter-delegations
  { election-id: uint, delegator: principal }
  {
    delegate: principal,
    delegation-block: uint,
    is-active: bool
  }
)

(define-map delegate-power
  { election-id: uint, delegate: principal }
  { total-delegated-votes: uint }
)

(define-public (delegate-vote (election-id uint) (delegate principal))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (delegator-eligibility (unwrap! (map-get? eligible-voters { election-id: election-id, voter: tx-sender }) err-unauthorized))
      (delegate-eligibility (unwrap! (map-get? eligible-voters { election-id: election-id, voter: delegate }) err-invalid-delegation))
      (existing-vote (map-get? voter-records { election-id: election-id, voter: tx-sender }))
      (existing-delegation (map-get? voter-delegations { election-id: election-id, delegator: tx-sender }))
      (current-delegate-power (default-to { total-delegated-votes: u0 } (map-get? delegate-power { election-id: election-id, delegate: delegate })))
    )
    (asserts! (get is-eligible delegator-eligibility) err-unauthorized)
    (asserts! (get is-eligible delegate-eligibility) err-invalid-delegation)
    (asserts! (not (is-eq tx-sender delegate)) err-cannot-delegate-to-self)
    (asserts! (get is-active election) err-election-not-active)
    (asserts! (< stacks-block-height (get end-block election)) err-election-ended)
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (is-none existing-delegation) err-already-exists)
    
    (map-set voter-delegations
      { election-id: election-id, delegator: tx-sender }
      {
        delegate: delegate,
        delegation-block: stacks-block-height,
        is-active: true
      }
    )
    
    (map-set delegate-power
      { election-id: election-id, delegate: delegate }
      { total-delegated-votes: (+ (get total-delegated-votes current-delegate-power) u1) }
    )
    
    (ok true)
  )
)

(define-public (revoke-delegation (election-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (delegation (unwrap! (map-get? voter-delegations { election-id: election-id, delegator: tx-sender }) err-delegation-not-found))
      (current-delegate-power (unwrap! (map-get? delegate-power { election-id: election-id, delegate: (get delegate delegation) }) err-not-found))
    )
    (asserts! (get is-active election) err-election-not-active)
    (asserts! (< stacks-block-height (get end-block election)) err-election-ended)
    (asserts! (get is-active delegation) err-delegation-not-found)
    
    (map-set voter-delegations
      { election-id: election-id, delegator: tx-sender }
      {
        delegate: (get delegate delegation),
        delegation-block: (get delegation-block delegation),
        is-active: false
      }
    )
    
    (map-set delegate-power
      { election-id: election-id, delegate: (get delegate delegation) }
      { total-delegated-votes: (- (get total-delegated-votes current-delegate-power) u1) }
    )
    
    (ok true)
  )
)

(define-public (cast-delegated-vote (election-id uint) (candidate-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (voter-eligibility (unwrap! (map-get? eligible-voters { election-id: election-id, voter: tx-sender }) err-unauthorized))
      (existing-vote (map-get? voter-records { election-id: election-id, voter: tx-sender }))
      (candidate (unwrap! (map-get? candidates { election-id: election-id, candidate-id: candidate-id }) err-invalid-candidate))
      (delegate-power-data (default-to { total-delegated-votes: u0 } (map-get? delegate-power { election-id: election-id, delegate: tx-sender })))
      (total-votes (+ u1 (get total-delegated-votes delegate-power-data)))
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
        vote-count: (+ (get vote-count candidate) total-votes)
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
        total-votes: (+ (get total-votes election) total-votes),
        creator: (get creator election)
      }
    )
    
    (ok total-votes)
  )
)

(define-read-only (get-delegation (election-id uint) (delegator principal))
  (map-get? voter-delegations { election-id: election-id, delegator: delegator })
)

(define-read-only (get-delegate-power (election-id uint) (delegate principal))
  (map-get? delegate-power { election-id: election-id, delegate: delegate })
)


(define-map election-audit-config
  { election-id: uint }
  {
    audit-enabled: bool,
    audit-start-block: uint,
    total-audit-entries: uint,
    verification-hash: (buff 32)
  }
)

(define-map audit-trail
  { election-id: uint, entry-id: uint }
  {
    action-type: (string-ascii 20),
    actor: principal,
    timestamp-block: uint,
    data-hash: (buff 32),
    previous-hash: (buff 32)
  }
)

(define-map vote-verification
  { election-id: uint, voter: principal }
  {
    verification-code: (buff 16),
    vote-timestamp: uint,
    is-verified: bool
  }
)

(define-map election-statistics
  { election-id: uint }
  {
    total-registered-voters: uint,
    total-votes-cast: uint,
    participation-rate: uint,
    votes-per-hour: uint,
    peak-voting-block: uint
  }
)

(define-public (enable-election-audit (election-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (existing-audit (map-get? election-audit-config { election-id: election-id }))
      (initial-hash (sha256 (concat (unwrap-panic (to-consensus-buff? election-id)) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (< stacks-block-height (get start-block election)) err-election-not-active)
    (asserts! (is-none existing-audit) err-audit-already-started)
    
    (map-set election-audit-config
      { election-id: election-id }
      {
        audit-enabled: true,
        audit-start-block: stacks-block-height,
        total-audit-entries: u0,
        verification-hash: initial-hash
      }
    )
    
    (try! (add-audit-entry election-id "AUDIT_ENABLED" tx-sender initial-hash))
    (ok true)
  )
)

(define-public (cast-audited-vote (election-id uint) (candidate-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (audit-config (unwrap! (map-get? election-audit-config { election-id: election-id }) err-audit-not-enabled))
      (voter-eligibility (unwrap! (map-get? eligible-voters { election-id: election-id, voter: tx-sender }) err-unauthorized))
      (existing-vote (map-get? voter-records { election-id: election-id, voter: tx-sender }))
      (candidate (unwrap! (map-get? candidates { election-id: election-id, candidate-id: candidate-id }) err-invalid-candidate))
      (vote-hash (sha256 (concat (unwrap-panic (to-consensus-buff? tx-sender)) (unwrap-panic (to-consensus-buff? candidate-id)))))
      (verification-code (as-max-len? (unwrap-panic (slice? vote-hash u0 u16)) u16))
    )
    (asserts! (get audit-enabled audit-config) err-audit-not-enabled)
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
    
    (map-set vote-verification
      { election-id: election-id, voter: tx-sender }
      {
        verification-code: (unwrap-panic verification-code),
        vote-timestamp: stacks-block-height,
        is-verified: false
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
    
    (try! (add-audit-entry election-id "VOTE_CAST" tx-sender vote-hash))
    (try! (update-election-statistics election-id))
    
    (ok (unwrap-panic verification-code))
  )
)

(define-public (verify-vote (election-id uint) (verification-code (buff 16)))
  (let
    (
      (verification-data (unwrap! (map-get? vote-verification { election-id: election-id, voter: tx-sender }) err-not-found))
      (voter-record (unwrap! (map-get? voter-records { election-id: election-id, voter: tx-sender }) err-not-found))
    )
    (asserts! (is-eq verification-code (get verification-code verification-data)) err-verification-failed)
    (asserts! (get has-voted voter-record) err-not-found)
    
    (map-set vote-verification
      { election-id: election-id, voter: tx-sender }
      {
        verification-code: (get verification-code verification-data),
        vote-timestamp: (get vote-timestamp verification-data),
        is-verified: true
      }
    )
    
    (try! (add-audit-entry election-id "VOTE_VERIFIED" tx-sender (get vote-hash voter-record)))
    (ok true)
  )
)

(define-public (generate-audit-report (election-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (audit-config (unwrap! (map-get? election-audit-config { election-id: election-id }) err-audit-not-enabled))
      (statistics (unwrap! (map-get? election-statistics { election-id: election-id }) err-not-found))
      (report-hash (sha256 (concat 
        (unwrap-panic (to-consensus-buff? election-id))
        (unwrap-panic (to-consensus-buff? (get total-audit-entries audit-config)))
      )))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (not (get is-active election)) err-election-not-ended)
    (asserts! (get audit-enabled audit-config) err-audit-not-enabled)
    
    (try! (add-audit-entry election-id "AUDIT_REPORT" tx-sender report-hash))
    
    (ok {
      election-id: election-id,
      total-entries: (get total-audit-entries audit-config),
      statistics: statistics,
      report-hash: report-hash
    })
  )
)

(define-public (validate-audit-entry (election-id uint) (entry-id uint))
  (let
    (
      (current-entry (unwrap! (map-get? audit-trail { election-id: election-id, entry-id: entry-id }) err-not-found))
      (audit-config (unwrap! (map-get? election-audit-config { election-id: election-id }) err-audit-not-enabled))
    )
    (asserts! (get audit-enabled audit-config) err-audit-not-enabled)
    (asserts! (<= entry-id (get total-audit-entries audit-config)) err-not-found)
    
    (if (is-eq entry-id u1)
      (ok true)
      (let
        (
          (previous-entry (unwrap! (map-get? audit-trail { election-id: election-id, entry-id: (- entry-id u1) }) err-not-found))
          (expected-hash (sha256 (concat (get previous-hash previous-entry) (get data-hash previous-entry))))
        )
        (ok (is-eq (get previous-hash current-entry) expected-hash))
      )
    )
  )
)

(define-private (add-audit-entry (election-id uint) (action-type (string-ascii 20)) (actor principal) (data-hash (buff 32)))
  (let
    (
      (audit-config (unwrap! (map-get? election-audit-config { election-id: election-id }) err-audit-not-enabled))
      (entry-id (+ (get total-audit-entries audit-config) u1))
      (previous-hash (get verification-hash audit-config))
      (new-hash (sha256 (concat previous-hash data-hash)))
    )
    (map-set audit-trail
      { election-id: election-id, entry-id: entry-id }
      {
        action-type: action-type,
        actor: actor,
        timestamp-block: stacks-block-height,
        data-hash: data-hash,
        previous-hash: previous-hash
      }
    )
    
    (map-set election-audit-config
      { election-id: election-id }
      {
        audit-enabled: (get audit-enabled audit-config),
        audit-start-block: (get audit-start-block audit-config),
        total-audit-entries: entry-id,
        verification-hash: new-hash
      }
    )
    
    (ok entry-id)
  )
)

(define-private (update-election-statistics (election-id uint))
  (let
    (
      (election (unwrap! (map-get? elections { election-id: election-id }) err-not-found))
      (existing-stats (default-to 
        { total-registered-voters: u100, total-votes-cast: u0, participation-rate: u0, votes-per-hour: u0, peak-voting-block: u0 }
        (map-get? election-statistics { election-id: election-id })
      ))
      (total-votes (get total-votes election))
      (registered-voters (get total-registered-voters existing-stats))
      (participation-rate (if (> registered-voters u0) (/ (* total-votes u100) registered-voters) u0))
      (blocks-elapsed (- stacks-block-height (get start-block election)))
      (votes-per-hour (if (> blocks-elapsed u0) (/ total-votes blocks-elapsed) u0))
    )
    (map-set election-statistics
      { election-id: election-id }
      {
        total-registered-voters: registered-voters,
        total-votes-cast: total-votes,
        participation-rate: participation-rate,
        votes-per-hour: votes-per-hour,
        peak-voting-block: (if (> total-votes (get total-votes-cast existing-stats)) stacks-block-height (get peak-voting-block existing-stats))
      }
    )
    (ok true)
  )
)

(define-read-only (get-audit-config (election-id uint))
  (map-get? election-audit-config { election-id: election-id })
)

(define-read-only (get-audit-entry (election-id uint) (entry-id uint))
  (map-get? audit-trail { election-id: election-id, entry-id: entry-id })
)

(define-read-only (get-vote-verification (election-id uint) (voter principal))
  (map-get? vote-verification { election-id: election-id, voter: voter })
)

(define-read-only (get-election-statistics (election-id uint))
  (map-get? election-statistics { election-id: election-id })
)

(define-read-only (get-audit-trail-hash (election-id uint))
  (match (map-get? election-audit-config { election-id: election-id })
    config (some (get verification-hash config))
    none
  )
)

(define-read-only (check-audit-integrity (election-id uint) (entry-id uint))
  (match (map-get? audit-trail { election-id: election-id, entry-id: entry-id })
    entry
    (if (is-eq entry-id u1)
      (some true)
      (match (map-get? audit-trail { election-id: election-id, entry-id: (- entry-id u1) })
        prev-entry
        (let
          (
            (expected-hash (sha256 (concat (get previous-hash prev-entry) (get data-hash prev-entry))))
          )
          (some (is-eq (get previous-hash entry) expected-hash))
        )
        none
      )
    )
    none
  )
)
