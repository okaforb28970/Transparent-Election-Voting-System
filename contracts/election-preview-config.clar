
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-election-not-active (err u104))
(define-constant err-preview-not-active (err u115))
(define-constant err-preview-already-voted (err u116))
(define-constant err-invalid-preview-candidate (err u117))

(define-map election-preview-config
  { election-id: uint }
  {
    preview-enabled: bool,
    preview-start-block: uint,
    preview-end-block: uint,
    total-preview-votes: uint
  }
)

(define-map preview-votes
  { election-id: uint, candidate-id: uint }
  { preview-vote-count: uint }
)

(define-map preview-voter-records
  { election-id: uint, voter: principal }
  {
    has-preview-voted: bool,
    preview-vote-block: uint,
    anonymous-hash: (buff 32)
  }
)

(define-public (enable-election-preview (election-id uint) (preview-duration-blocks uint))
  (let
    (
      (election (unwrap! (contract-call? .Transparent-Election get-election election-id) err-not-found))
      (existing-preview (map-get? election-preview-config { election-id: election-id }))
      (preview-start-block stacks-block-height)
      (preview-end-block (+ stacks-block-height preview-duration-blocks))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (< preview-end-block (get start-block election)) err-election-not-active)
    (asserts! (is-none existing-preview) err-already-exists)
    
    (map-set election-preview-config
      { election-id: election-id }
      {
        preview-enabled: true,
        preview-start-block: preview-start-block,
        preview-end-block: preview-end-block,
        total-preview-votes: u0
      }
    )
    (ok true)
  )
)

(define-public (cast-preview-vote (election-id uint) (candidate-id uint))
  (let
    (
      (preview-config (unwrap! (map-get? election-preview-config { election-id: election-id }) err-preview-not-active))
      (existing-preview-vote (map-get? preview-voter-records { election-id: election-id, voter: tx-sender }))
      (candidate (unwrap! (contract-call? .Transparent-Election get-candidate election-id candidate-id) err-invalid-preview-candidate))
      (current-preview-votes (default-to { preview-vote-count: u0 } (map-get? preview-votes { election-id: election-id, candidate-id: candidate-id })))
      (anonymous-hash (sha256 (concat (unwrap-panic (to-consensus-buff? stacks-block-height)) (unwrap-panic (to-consensus-buff? candidate-id)))))
    )
    (asserts! (get preview-enabled preview-config) err-preview-not-active)
    (asserts! (>= stacks-block-height (get preview-start-block preview-config)) err-preview-not-active)
    (asserts! (< stacks-block-height (get preview-end-block preview-config)) err-preview-not-active)
    (asserts! (is-none existing-preview-vote) err-preview-already-voted)
    
    (map-set preview-voter-records
      { election-id: election-id, voter: tx-sender }
      {
        has-preview-voted: true,
        preview-vote-block: stacks-block-height,
        anonymous-hash: anonymous-hash
      }
    )
    
    (map-set preview-votes
      { election-id: election-id, candidate-id: candidate-id }
      { preview-vote-count: (+ (get preview-vote-count current-preview-votes) u1) }
    )
    
    (map-set election-preview-config
      { election-id: election-id }
      {
        preview-enabled: (get preview-enabled preview-config),
        preview-start-block: (get preview-start-block preview-config),
        preview-end-block: (get preview-end-block preview-config),
        total-preview-votes: (+ (get total-preview-votes preview-config) u1)
      }
    )
    
    (ok true)
  )
)

(define-read-only (get-preview-config (election-id uint))
  (map-get? election-preview-config { election-id: election-id })
)

(define-read-only (get-preview-votes (election-id uint) (candidate-id uint))
  (map-get? preview-votes { election-id: election-id, candidate-id: candidate-id })
)

(define-read-only (get-preview-voter-status (election-id uint) (voter principal))
  (map-get? preview-voter-records { election-id: election-id, voter: voter })
)

(define-read-only (is-preview-active (election-id uint))
  (match (map-get? election-preview-config { election-id: election-id })
    config
    (and 
      (get preview-enabled config)
      (>= stacks-block-height (get preview-start-block config))
      (< stacks-block-height (get preview-end-block config))
    )
    false
  )
)