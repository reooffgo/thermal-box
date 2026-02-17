;; ============================================================
;; THERMAL BOX - Core Smart Contract
;; Stacks / Clarity
;; ============================================================
;; Covers:
;;   - THERM fungible token
;;   - Thermal Equipment NFTs (SIP-009-style)
;;   - Thermal Zone registry & Temperature Oracle
;;   - Equipment staking (communal mining rigs)
;;   - Solo expedition rewards
;;   - Thermal Council governance (proposals & voting)
;;   - Equipment degradation mechanics
;; ============================================================

;; -------------------------------------------------------
;; SIP-010 Fungible Token - THERM
;; -------------------------------------------------------
(define-fungible-token THERM)

(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-NAME "Thermal Energy Token")
(define-constant TOKEN-SYMBOL "THERM")
(define-constant TOKEN-URI (some u"https://thermalbox.io/token"))

(define-read-only (get-name)          (ok TOKEN-NAME))
(define-read-only (get-symbol)        (ok TOKEN-SYMBOL))
(define-read-only (get-decimals)      (ok TOKEN-DECIMALS))
(define-read-only (get-token-uri)     (ok TOKEN-URI))
(define-read-only (get-total-supply)  (ok (ft-get-supply THERM)))
(define-read-only (get-balance (who principal))
  (ok (ft-get-balance THERM who)))

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (match memo m (begin (print m) true) true)
    (ft-transfer? THERM amount sender recipient)))

;; -------------------------------------------------------
;; SIP-009 Non-Fungible Token - Thermal Equipment
;; -------------------------------------------------------
(define-non-fungible-token thermal-equipment uint)

(define-data-var last-equipment-id uint u0)

;; Equipment metadata stored on-chain
(define-map equipment-data
  { equipment-id: uint }
  {
    name:          (string-ascii 64),
    tier:          uint,        ;; 1-5 (higher = better heat resistance)
    heat-capacity: uint,        ;; max temperature it can handle (in units)
    durability:    uint,        ;; 0-1000; 0 = destroyed
    staked:        bool,
    owner:         principal
  })

;; -------------------------------------------------------
;; Error constants
;; -------------------------------------------------------
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-NOT-FOUND             (err u101))
(define-constant ERR-ALREADY-STAKED        (err u102))
(define-constant ERR-NOT-STAKED            (err u103))
(define-constant ERR-EQUIPMENT-DESTROYED   (err u104))
(define-constant ERR-ZONE-TOO-HOT          (err u105))
(define-constant ERR-INVALID-AMOUNT        (err u106))
(define-constant ERR-PROPOSAL-ACTIVE       (err u107))
(define-constant ERR-PROPOSAL-NOT-FOUND    (err u108))
(define-constant ERR-ALREADY-VOTED         (err u109))
(define-constant ERR-PROPOSAL-CLOSED       (err u110))

;; -------------------------------------------------------
;; Contract owner / admin
;; -------------------------------------------------------
(define-data-var contract-owner principal tx-sender)

(define-read-only (get-contract-owner) (var-get contract-owner))

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))))

;; -------------------------------------------------------
;; Temperature Oracle - Thermal Zones
;; -------------------------------------------------------
(define-map thermal-zones
  { zone-id: uint }
  {
    name:        (string-ascii 32),
    temperature: uint,   ;; current zone temperature
    loot-bonus:  uint,   ;; bonus multiplier (basis points, 10000 = 1x)
    active:      bool
  })

(define-data-var zone-count uint u0)

(define-public (register-zone (name (string-ascii 32)) (temperature uint) (loot-bonus uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (let ((new-id (+ (var-get zone-count) u1)))
      (map-set thermal-zones { zone-id: new-id }
        { name: name, temperature: temperature, loot-bonus: loot-bonus, active: true })
      (var-set zone-count new-id)
      (ok new-id))))

;; Oracle update (called by trusted oracle principal or owner)
(define-data-var oracle-address principal tx-sender)

(define-public (set-oracle-address (addr principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set oracle-address addr))))

(define-public (update-zone-temperature (zone-id uint) (new-temperature uint))
  (begin
    (asserts! (or (is-eq tx-sender (var-get oracle-address))
                  (is-eq tx-sender (var-get contract-owner)))
              ERR-NOT-AUTHORIZED)
    (match (map-get? thermal-zones { zone-id: zone-id })
      zone (begin
              (map-set thermal-zones { zone-id: zone-id }
                (merge zone { temperature: new-temperature }))
              (ok true))
      ERR-NOT-FOUND)))

(define-read-only (get-zone (zone-id uint))
  (ok (map-get? thermal-zones { zone-id: zone-id })))

;; -------------------------------------------------------
;; Equipment Minting
;; -------------------------------------------------------
(define-public (mint-equipment
    (recipient principal)
    (name (string-ascii 64))
    (tier uint)
    (heat-capacity uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (let ((new-id (+ (var-get last-equipment-id) u1)))
      (try! (nft-mint? thermal-equipment new-id recipient))
      (map-set equipment-data { equipment-id: new-id }
        { name:          name,
          tier:          tier,
          heat-capacity: heat-capacity,
          durability:    u1000,
          staked:        false,
          owner:         recipient })
      (var-set last-equipment-id new-id)
      (ok new-id))))

(define-read-only (get-equipment (equipment-id uint))
  (ok (map-get? equipment-data { equipment-id: equipment-id })))

(define-read-only (get-owner (equipment-id uint))
  (ok (nft-get-owner? thermal-equipment equipment-id)))

;; -------------------------------------------------------
;; Equipment Staking - Communal Mining Rig
;; -------------------------------------------------------
(define-map staking-records
  { equipment-id: uint }
  { staked-at: uint, owner: principal, zone-id: uint })

;; Emission rate: THERM micro-units per block per tier
(define-constant EMISSION-RATE-PER-TIER u50)

(define-public (stake-equipment (equipment-id uint) (zone-id uint))
  (let ((equip (unwrap! (map-get? equipment-data { equipment-id: equipment-id }) ERR-NOT-FOUND))
        (zone  (unwrap! (map-get? thermal-zones  { zone-id: zone-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner equip)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get staked equip))             ERR-ALREADY-STAKED)
    (asserts! (> (get durability equip) u0)        ERR-EQUIPMENT-DESTROYED)
    ;; Equipment tier must match zone temperature threshold
    (asserts! (>= (get heat-capacity equip) (get temperature zone)) ERR-ZONE-TOO-HOT)
    (map-set equipment-data { equipment-id: equipment-id }
      (merge equip { staked: true }))
    (map-set staking-records { equipment-id: equipment-id }
      { staked-at: block-height, owner: tx-sender, zone-id: zone-id })
    (ok true)))

(define-public (unstake-equipment (equipment-id uint))
  (let ((equip  (unwrap! (map-get? equipment-data   { equipment-id: equipment-id }) ERR-NOT-FOUND))
        (record (unwrap! (map-get? staking-records   { equipment-id: equipment-id }) ERR-NOT-STAKED))
        (zone   (unwrap! (map-get? thermal-zones { zone-id: (get zone-id record) }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner equip)) ERR-NOT-AUTHORIZED)
    (asserts! (get staked equip)                  ERR-NOT-STAKED)
    (let ((blocks-staked (- block-height (get staked-at record))))
      (let ((raw-reward (* blocks-staked (* EMISSION-RATE-PER-TIER (get tier equip)))))
        (let ((reward (/ (* raw-reward (get loot-bonus zone)) u10000)))
          (let ((degradation (/ (* blocks-staked (get temperature zone)) u500)))
            (let ((new-durability (if (> degradation (get durability equip))
                                    u0
                                    (- (get durability equip) degradation))))
              (try! (ft-mint? THERM reward tx-sender))
              (map-delete staking-records { equipment-id: equipment-id })
              (map-set equipment-data { equipment-id: equipment-id }
                (merge equip { staked: false, durability: new-durability }))
              (ok { reward: reward, durability: new-durability }))))))))


;; -------------------------------------------------------
;; Solo Expedition
;; -------------------------------------------------------
;; A lightweight single-tx expedition: spend THERM entry fee,
;; equipment takes heat damage, receive amplified reward if
;; equipment survives.

(define-constant EXPEDITION-ENTRY-FEE  u1000000)  ;; 1 THERM
(define-constant EXPEDITION-BASE-REWARD u5000000) ;; 5 THERM

(define-public (start-solo-expedition (equipment-id uint) (zone-id uint))
  (let ((equip (unwrap! (map-get? equipment-data { equipment-id: equipment-id }) ERR-NOT-FOUND))
        (zone  (unwrap! (map-get? thermal-zones  { zone-id: zone-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner equip)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get staked equip))             ERR-ALREADY-STAKED)
    (asserts! (> (get durability equip) u0)        ERR-EQUIPMENT-DESTROYED)
    ;; Pay entry fee (burned to contract owner as treasury placeholder)
    (try! (ft-transfer? THERM EXPEDITION-ENTRY-FEE tx-sender (var-get contract-owner)))
    (let ((temp-ratio (/ (* (get temperature zone) u100) (get heat-capacity equip))))
      (let ((multiplier (if (>= temp-ratio u100) u150 (+ u100 (/ temp-ratio u2)))))
        (let ((reward (/ (* EXPEDITION-BASE-REWARD multiplier) u100)))
          (let ((degradation (/ temp-ratio u5)))
            (let ((new-dur (if (> degradation (get durability equip))
                              u0
                              (- (get durability equip) degradation))))
              (try! (ft-mint? THERM reward tx-sender))
              (map-set equipment-data { equipment-id: equipment-id }
                (merge equip { durability: new-dur }))
              (ok { reward: reward, new-durability: new-dur, multiplier: multiplier }))))))))

;; -------------------------------------------------------
;; Equipment Repair
;; -------------------------------------------------------
(define-constant REPAIR-COST-PER-POINT u500) ;; 0.0005 THERM per durability point

(define-public (repair-equipment (equipment-id uint) (durability-to-restore uint))
  (let ((equip (unwrap! (map-get? equipment-data { equipment-id: equipment-id }) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner equip)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get staked equip))             ERR-ALREADY-STAKED)
    (asserts! (> durability-to-restore u0)         ERR-INVALID-AMOUNT)
    (let ((cost (* durability-to-restore REPAIR-COST-PER-POINT)))
      (let ((new-dur (if (> (+ (get durability equip) durability-to-restore) u1000)
                        u1000
                        (+ (get durability equip) durability-to-restore))))
        (try! (ft-transfer? THERM cost tx-sender (var-get contract-owner)))
        (map-set equipment-data { equipment-id: equipment-id }
          (merge equip { durability: new-dur }))
        (ok { cost: cost, new-durability: new-dur })))))

;; -------------------------------------------------------
;; Thermal Council - Governance
;; -------------------------------------------------------
(define-map proposals
  { proposal-id: uint }
  {
    proposer:    principal,
    description: (string-ascii 256),
    yes-votes:   uint,
    no-votes:    uint,
    end-block:   uint,
    executed:    bool
  })

(define-map votes-cast
  { proposal-id: uint, voter: principal }
  { voted: bool })

(define-data-var proposal-count uint u0)
(define-constant PROPOSAL-DURATION u1440)   ;; ~10 days at 1 block/min
(define-constant PROPOSAL-THRESHOLD u10000) ;; min THERM to propose

(define-public (create-proposal (description (string-ascii 256)))
  (begin
    (asserts! (>= (ft-get-balance THERM tx-sender) PROPOSAL-THRESHOLD) ERR-NOT-AUTHORIZED)
    (let ((new-id (+ (var-get proposal-count) u1)))
      (map-set proposals { proposal-id: new-id }
        { proposer:    tx-sender,
          description: description,
          yes-votes:   u0,
          no-votes:    u0,
          end-block:   (+ block-height PROPOSAL-DURATION),
          executed:    false })
      (var-set proposal-count new-id)
      (ok new-id))))

(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
  (let ((proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
        (balance  (ft-get-balance THERM tx-sender)))
    (asserts! (<= block-height (get end-block proposal)) ERR-PROPOSAL-CLOSED)
    (asserts! (is-none (map-get? votes-cast { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    (asserts! (> balance u0) ERR-NOT-AUTHORIZED)
    (map-set votes-cast { proposal-id: proposal-id, voter: tx-sender } { voted: true })
    (if vote-yes
      (map-set proposals { proposal-id: proposal-id }
        { proposer:    (get proposer proposal),
          description: (get description proposal),
          yes-votes:   (+ (get yes-votes proposal) balance),
          no-votes:    (get no-votes proposal),
          end-block:   (get end-block proposal),
          executed:    (get executed proposal) })
      (map-set proposals { proposal-id: proposal-id }
        { proposer:    (get proposer proposal),
          description: (get description proposal),
          yes-votes:   (get yes-votes proposal),
          no-votes:    (+ (get no-votes proposal) balance),
          end-block:   (get end-block proposal),
          executed:    (get executed proposal) }))
    (ok true)))

(define-read-only (get-proposal (proposal-id uint))
  (ok (map-get? proposals { proposal-id: proposal-id })))

(define-read-only (proposal-passed (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    p (ok (and (> block-height (get end-block p))
               (> (get yes-votes p) (get no-votes p))))
    ERR-PROPOSAL-NOT-FOUND))

;; -------------------------------------------------------
;; Treasury / Carbon Offset Token Stub
;; -------------------------------------------------------
;; Placeholder for carbon offset tokenization partnership
(define-map carbon-credits { holder: principal } { credits: uint })

(define-public (issue-carbon-credits (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (match (map-get? carbon-credits { holder: recipient })
      existing (map-set carbon-credits { holder: recipient }
                  { credits: (+ (get credits existing) amount) })
      (map-set carbon-credits { holder: recipient } { credits: amount }))
    (ok amount)))

(define-read-only (get-carbon-credits (holder principal))
  (ok (default-to { credits: u0 } (map-get? carbon-credits { holder: holder }))))

;; -------------------------------------------------------
;; Utility read-only helpers
;; -------------------------------------------------------
(define-read-only (get-last-equipment-id) (var-get last-equipment-id))
(define-read-only (get-zone-count)        (var-get zone-count))
(define-read-only (get-proposal-count)    (var-get proposal-count))

(define-read-only (get-staking-record (equipment-id uint))
  (ok (map-get? staking-records { equipment-id: equipment-id })))

;; Estimate pending reward without unstaking
(define-read-only (estimate-staking-reward (equipment-id uint))
  (match (map-get? staking-records { equipment-id: equipment-id })
    record
      (match (map-get? equipment-data { equipment-id: equipment-id })
        equip
          (match (map-get? thermal-zones { zone-id: (get zone-id record) })
            zone
              (let ((blocks-staked (- block-height (get staked-at record))))
                (let ((raw (* blocks-staked (* EMISSION-RATE-PER-TIER (get tier equip)))))
                  (let ((reward (/ (* raw (get loot-bonus zone)) u10000)))
                    (ok reward))))
            ERR-NOT-FOUND)
        ERR-NOT-FOUND)
    ERR-NOT-STAKED))
