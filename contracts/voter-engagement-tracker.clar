(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-tracked (err u124))
(define-constant err-invalid-election (err u125))

(define-map voter-engagement
  { voter: principal }
  {
    total-elections-voted: uint,
    current-streak: uint,
    longest-streak: uint,
    early-votes-count: uint,
    last-vote-election-id: uint,
    engagement-level: uint
  }
)

(define-map election-participation-tracker
  { election-id: uint, voter: principal }
  {
    voted-early: bool,
    participation-tracked: bool,
    vote-timing-percentile: uint
  }
)

(define-map engagement-milestones
  { voter: principal, milestone-id: uint }
  {
    milestone-type: (string-ascii 30),
    achieved-at-block: uint,
    election-id: uint
  }
)

(define-map voter-milestone-counter
  { voter: principal }
  { total-milestones: uint }
)

(define-public (track-vote-participation (election-id uint) (voter principal))
  (let
    (
      (election (unwrap! (contract-call? .Transparent-Election get-election election-id) err-not-found))
      (voter-record (unwrap! (contract-call? .Transparent-Election get-voter-status election-id voter) err-not-found))
      (already-tracked (map-get? election-participation-tracker { election-id: election-id, voter: voter }))
      (current-engagement (default-to 
        { total-elections-voted: u0, current-streak: u0, longest-streak: u0, early-votes-count: u0, last-vote-election-id: u0, engagement-level: u0 }
        (map-get? voter-engagement { voter: voter })))
      (blocks-into-election (- stacks-block-height (get start-block election)))
      (total-election-duration (- (get end-block election) (get start-block election)))
      (voted-early (< blocks-into-election (/ total-election-duration u2)))
      (new-streak (if (is-eq (+ (get last-vote-election-id current-engagement) u1) election-id)
                    (+ (get current-streak current-engagement) u1)
                    u1))
      (new-longest (if (> new-streak (get longest-streak current-engagement)) new-streak (get longest-streak current-engagement)))
      (new-early-count (if voted-early (+ (get early-votes-count current-engagement) u1) (get early-votes-count current-engagement)))
      (new-total (+ (get total-elections-voted current-engagement) u1))
      (engagement-score (+ (* new-total u10) (* new-streak u5) (* new-early-count u3)))
    )
    (asserts! (get has-voted voter-record) err-not-found)
    (asserts! (is-none already-tracked) err-already-tracked)
    
    (map-set election-participation-tracker
      { election-id: election-id, voter: voter }
      {
        voted-early: voted-early,
        participation-tracked: true,
        vote-timing-percentile: (/ (* blocks-into-election u100) total-election-duration)
      }
    )
    
    (map-set voter-engagement
      { voter: voter }
      {
        total-elections-voted: new-total,
        current-streak: new-streak,
        longest-streak: new-longest,
        early-votes-count: new-early-count,
        last-vote-election-id: election-id,
        engagement-level: engagement-score
      }
    )
    
    (unwrap! (check-and-award-milestones voter election-id new-total new-streak new-early-count) err-not-found)
    (ok engagement-score)
  )
)

(define-private (check-and-award-milestones (voter principal) (election-id uint) (total-votes uint) (streak uint) (early-votes uint))
  (let
    (
      (milestone-data (default-to { total-milestones: u0 } (map-get? voter-milestone-counter { voter: voter })))
      (current-count (get total-milestones milestone-data))
    )
    (begin
      (if (is-eq total-votes u10)
        (begin
          (map-set engagement-milestones
            { voter: voter, milestone-id: (+ current-count u1) }
            { milestone-type: "VETERAN_VOTER", achieved-at-block: stacks-block-height, election-id: election-id })
          (map-set voter-milestone-counter { voter: voter } { total-milestones: (+ current-count u1) })
          true)
        (if (is-eq streak u5)
          (begin
            (map-set engagement-milestones
              { voter: voter, milestone-id: (+ current-count u1) }
              { milestone-type: "STREAK_MASTER", achieved-at-block: stacks-block-height, election-id: election-id })
            (map-set voter-milestone-counter { voter: voter } { total-milestones: (+ current-count u1) })
            true)
          false))
      (ok true)
    )
  )
)

(define-read-only (get-voter-engagement (voter principal))
  (map-get? voter-engagement { voter: voter })
)

(define-read-only (get-participation-record (election-id uint) (voter principal))
  (map-get? election-participation-tracker { election-id: election-id, voter: voter })
)

(define-read-only (get-voter-milestone (voter principal) (milestone-id uint))
  (map-get? engagement-milestones { voter: voter, milestone-id: milestone-id })
)

(define-read-only (get-total-milestones (voter principal))
  (map-get? voter-milestone-counter { voter: voter })
)
