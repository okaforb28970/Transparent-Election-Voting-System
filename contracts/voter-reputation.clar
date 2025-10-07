(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-reputation (err u118))
(define-constant err-reputation-not-enabled (err u119))

(define-map voter-reputation
  { voter: principal }
  {
    total-elections-participated: uint,
    total-majority-alignments: uint,
    reputation-score: uint,
    last-updated-block: uint
  }
)

(define-map election-reputation-config
  { election-id: uint }
  {
    reputation-enabled: bool,
    base-weight: uint,
    max-weight-multiplier: uint
  }
)

(define-map reputation-weighted-votes
  { election-id: uint, voter: principal }
  {
    base-votes: uint,
    reputation-bonus: uint,
    total-weighted-votes: uint
  }
)

(define-public (enable-reputation-voting (election-id uint) (base-weight uint) (max-multiplier uint))
  (let
    (
      (election (unwrap! (contract-call? .Transparent-Election get-election election-id) err-not-found))
      (existing-config (map-get? election-reputation-config { election-id: election-id }))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (< stacks-block-height (get start-block election)) err-unauthorized)
    (asserts! (is-none existing-config) err-unauthorized)
    
    (map-set election-reputation-config
      { election-id: election-id }
      {
        reputation-enabled: true,
        base-weight: base-weight,
        max-weight-multiplier: max-multiplier
      }
    )
    (ok true)
  )
)

(define-public (cast-reputation-weighted-vote (election-id uint) (candidate-id uint))
  (let
    (
      (reputation-config (unwrap! (map-get? election-reputation-config { election-id: election-id }) err-reputation-not-enabled))
      (voter-rep (default-to { total-elections-participated: u0, total-majority-alignments: u0, reputation-score: u100, last-updated-block: u0 } 
                              (map-get? voter-reputation { voter: tx-sender })))
      (base-weight (get base-weight reputation-config))
      (weight-multiplier (calculate-weight-multiplier (get reputation-score voter-rep) (get max-weight-multiplier reputation-config)))
      (total-weighted-votes (/ (* base-weight weight-multiplier) u100))
    )
    (asserts! (get reputation-enabled reputation-config) err-reputation-not-enabled)
    
    (try! (contract-call? .Transparent-Election cast-vote election-id candidate-id))
    
    (map-set reputation-weighted-votes
      { election-id: election-id, voter: tx-sender }
      {
        base-votes: base-weight,
        reputation-bonus: (- total-weighted-votes base-weight),
        total-weighted-votes: total-weighted-votes
      }
    )
    
    (ok total-weighted-votes)
  )
)

(define-public (update-voter-reputation (voter principal) (election-id uint) (won-majority bool))
  (let
    (
      (election (unwrap! (contract-call? .Transparent-Election get-election election-id) err-not-found))
      (current-rep (default-to { total-elections-participated: u0, total-majority-alignments: u0, reputation-score: u100, last-updated-block: u0 } 
                                (map-get? voter-reputation { voter: voter })))
      (new-participations (+ (get total-elections-participated current-rep) u1))
      (new-alignments (if won-majority (+ (get total-majority-alignments current-rep) u1) (get total-majority-alignments current-rep)))
      (alignment-rate (/ (* new-alignments u100) new-participations))
      (participation-bonus (if (< (/ new-participations u2) u50) (/ new-participations u2) u50))
      (new-score (if (< (+ u50 alignment-rate participation-bonus) u300) (+ u50 alignment-rate participation-bonus) u300))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (not (get is-active election)) err-unauthorized)
    
    (map-set voter-reputation
      { voter: voter }
      {
        total-elections-participated: new-participations,
        total-majority-alignments: new-alignments,
        reputation-score: new-score,
        last-updated-block: stacks-block-height
      }
    )
    (ok new-score)
  )
)

(define-private (calculate-weight-multiplier (reputation-score uint) (max-multiplier uint))
  (let
    (
      (score-factor (/ reputation-score u100))
      (multiplier (+ u100 (/ (* (- score-factor u100) (- max-multiplier u100)) u100)))
    )
    (if (< multiplier u100) u100 (if (> multiplier max-multiplier) max-multiplier multiplier))
  )
)

(define-read-only (get-voter-reputation (voter principal))
  (map-get? voter-reputation { voter: voter })
)

(define-read-only (get-reputation-config (election-id uint))
  (map-get? election-reputation-config { election-id: election-id })
)

(define-read-only (get-weighted-vote-record (election-id uint) (voter principal))
  (map-get? reputation-weighted-votes { election-id: election-id, voter: voter })
)

(define-read-only (calculate-voting-weight (voter principal) (election-id uint))
  (match (map-get? election-reputation-config { election-id: election-id })
    config
    (if (get reputation-enabled config)
      (let
        (
          (voter-rep (default-to { total-elections-participated: u0, total-majority-alignments: u0, reputation-score: u100, last-updated-block: u0 } 
                                  (map-get? voter-reputation { voter: voter })))
          (weight-multiplier (calculate-weight-multiplier (get reputation-score voter-rep) (get max-weight-multiplier config)))
        )
        (some (/ (* (get base-weight config) weight-multiplier) u100))
      )
      (some u1)
    )
    (some u1)
  )
)
