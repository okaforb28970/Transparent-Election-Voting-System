(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-suspended (err u120))
(define-constant err-not-suspended (err u121))
(define-constant err-suspension-expired (err u122))
(define-constant err-invalid-duration (err u123))

(define-map suspension-status
  { election-id: uint }
  {
    is-suspended: bool,
    suspension-block: uint,
    resume-block: uint,
    suspension-count: uint,
    last-resumed-block: uint
  }
)

(define-map suspension-events
  { election-id: uint, event-id: uint }
  {
    action: (string-ascii 10),
    reason: (string-ascii 200),
    executor: principal,
    timestamp-block: uint,
    event-hash: (buff 32)
  }
)

(define-map suspension-config
  { election-id: uint }
  {
    max-suspension-duration: uint,
    authorized-suspenders: (list 5 principal),
    total-events: uint
  }
)

(define-public (initialize-suspension-system (election-id uint) (max-duration uint) (suspenders (list 5 principal)))
  (let
    (
      (election (unwrap! (contract-call? .Transparent-Election get-election election-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator election)) err-unauthorized)
    (asserts! (> max-duration u0) err-invalid-duration)
    
    (map-set suspension-config
      { election-id: election-id }
      {
        max-suspension-duration: max-duration,
        authorized-suspenders: suspenders,
        total-events: u0
      }
    )
    (ok true)
  )
)

(define-public (suspend-election (election-id uint) (reason (string-ascii 200)) (duration-blocks uint))
  (let
    (
      (config (unwrap! (map-get? suspension-config { election-id: election-id }) err-not-found))
      (current-status (map-get? suspension-status { election-id: election-id }))
      (event-hash (sha256 (concat (unwrap-panic (to-consensus-buff? election-id)) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
    )
    (asserts! (is-some (index-of? (get authorized-suspenders config) tx-sender)) err-unauthorized)
    (asserts! (<= duration-blocks (get max-suspension-duration config)) err-invalid-duration)
    (asserts! (or (is-none current-status) (not (get is-suspended (unwrap-panic current-status)))) err-already-suspended)
    
    (map-set suspension-status
      { election-id: election-id }
      {
        is-suspended: true,
        suspension-block: stacks-block-height,
        resume-block: (+ stacks-block-height duration-blocks),
        suspension-count: (+ (default-to u0 (get suspension-count current-status)) u1),
        last-resumed-block: (default-to u0 (get last-resumed-block current-status))
      }
    )
    
    (try! (log-suspension-event election-id "SUSPEND" reason event-hash))
    (ok true)
  )
)

(define-public (resume-election (election-id uint) (reason (string-ascii 200)))
  (let
    (
      (config (unwrap! (map-get? suspension-config { election-id: election-id }) err-not-found))
      (status (unwrap! (map-get? suspension-status { election-id: election-id }) err-not-suspended))
      (event-hash (sha256 (concat (unwrap-panic (to-consensus-buff? election-id)) (unwrap-panic (to-consensus-buff? stacks-block-height)))))
    )
    (asserts! (is-some (index-of? (get authorized-suspenders config) tx-sender)) err-unauthorized)
    (asserts! (get is-suspended status) err-not-suspended)
    
    (map-set suspension-status
      { election-id: election-id }
      {
        is-suspended: false,
        suspension-block: (get suspension-block status),
        resume-block: (get resume-block status),
        suspension-count: (get suspension-count status),
        last-resumed-block: stacks-block-height
      }
    )
    
    (try! (log-suspension-event election-id "RESUME" reason event-hash))
    (ok true)
  )
)

(define-private (log-suspension-event (election-id uint) (action (string-ascii 10)) (reason (string-ascii 200)) (event-hash (buff 32)))
  (let
    (
      (config (unwrap! (map-get? suspension-config { election-id: election-id }) err-not-found))
      (event-id (+ (get total-events config) u1))
    )
    (map-set suspension-events
      { election-id: election-id, event-id: event-id }
      {
        action: action,
        reason: reason,
        executor: tx-sender,
        timestamp-block: stacks-block-height,
        event-hash: event-hash
      }
    )
    
    (map-set suspension-config
      { election-id: election-id }
      {
        max-suspension-duration: (get max-suspension-duration config),
        authorized-suspenders: (get authorized-suspenders config),
        total-events: event-id
      }
    )
    (ok event-id)
  )
)

(define-read-only (get-suspension-status (election-id uint))
  (map-get? suspension-status { election-id: election-id })
)

(define-read-only (get-suspension-config (election-id uint))
  (map-get? suspension-config { election-id: election-id })
)

(define-read-only (get-suspension-event (election-id uint) (event-id uint))
  (map-get? suspension-events { election-id: election-id, event-id: event-id })
)

(define-read-only (is-election-suspended (election-id uint))
  (match (map-get? suspension-status { election-id: election-id })
    status (get is-suspended status)
    false
  )
)
