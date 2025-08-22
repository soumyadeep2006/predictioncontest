;; Prediction Contests Smart Contract
;; A decentralized platform for creating and participating in prediction contests

;; Define the main data structures
(define-map contests 
  uint 
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    end-block: uint,
    total-pool: uint,
    is-resolved: bool,
    winning-option: (optional uint)
  })

(define-map predictions 
  {contest-id: uint, user: principal}
  {
    option: uint,
    stake: uint,
    block-height: uint
  })

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-contest-not-found (err u101))
(define-constant err-contest-ended (err u102))
(define-constant err-contest-active (err u103))
(define-constant err-invalid-option (err u104))
(define-constant err-insufficient-stake (err u105))
(define-constant err-already-predicted (err u106))
(define-constant err-not-resolved (err u107))
(define-constant err-invalid-amount (err u108))

;; Data variables
(define-data-var next-contest-id uint u1)
(define-data-var platform-fee-rate uint u50) ;; 5% fee (50/1000)

;; Create a new prediction contest
(define-public (create-contest 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (duration-blocks uint))
  (let ((contest-id (var-get next-contest-id))
        (end-block (+ block-height duration-blocks)))
    (begin
      (asserts! (> duration-blocks u0) err-invalid-amount)
      (map-set contests contest-id
        {
          creator: tx-sender,
          title: title,
          description: description,
          end-block: end-block,
          total-pool: u0,
          is-resolved: false,
          winning-option: none
        })
      (var-set next-contest-id (+ contest-id u1))
      (print {action: "contest-created", contest-id: contest-id, creator: tx-sender})
      (ok contest-id))))

;; Place a prediction on a contest
(define-public (place-prediction (contest-id uint) (option uint) (stake uint))
  (let ((contest (unwrap! (map-get? contests contest-id) err-contest-not-found))
        (prediction-key {contest-id: contest-id, user: tx-sender}))
    (begin
      ;; Validate contest is active
      (asserts! (< block-height (get end-block contest)) err-contest-ended)
      (asserts! (is-eq (get is-resolved contest) false) err-contest-active)
      
      ;; Validate prediction parameters
      (asserts! (> stake u0) err-insufficient-stake)
      (asserts! (or (is-eq option u0) (is-eq option u1)) err-invalid-option)
      (asserts! (is-none (map-get? predictions prediction-key)) err-already-predicted)
      
      ;; Transfer STX stake to contract
      (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))
      
      ;; Store prediction
      (map-set predictions prediction-key
        {
          option: option,
          stake: stake,
          block-height: block-height
        })
      
      ;; Update contest total pool
      (map-set contests contest-id
        (merge contest {total-pool: (+ (get total-pool contest) stake)}))
      
      (print {action: "prediction-placed", contest-id: contest-id, user: tx-sender, option: option, stake: stake})
      (ok true))))

;; Read-only functions
(define-read-only (get-contest (contest-id uint))
  (ok (map-get? contests contest-id)))

(define-read-only (get-prediction (contest-id uint) (user principal))
  (ok (map-get? predictions {contest-id: contest-id, user: user})))

(define-read-only (get-next-contest-id)
  (ok (var-get next-contest-id)))

(define-read-only (get-platform-fee-rate)
  (ok (var-get platform-fee-rate)))