;; ContentVault: Tokenized Subscription Service
;; Description: Smart contract for managing subscription-based access to digital content with auto-renewal and trial periods

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-initialized (err u101))
(define-constant err-not-initialized (err u102))
(define-constant err-subscription-expired (err u103))
(define-constant err-invalid-tier (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-auto-renew-failed (err u106))
(define-constant err-trial-expired (err u107))
(define-constant err-invalid-subscriber (err u108))
(define-constant err-invalid-block-height (err u109))

;; Data Variables
(define-data-var contract-initialized bool false)

;; Data Maps
(define-map subscriptions
    principal
    {tier: (string-ascii 6),
     expiration: uint,
     active: bool,
     auto-renew: bool,
     trial-used: bool,
     trial-expiration: uint})

(define-map subscription-tiers
    (string-ascii 6)
    {price: uint,
     duration: uint,
     trial-duration: uint,
     benefits: (string-ascii 50)})

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner))

(define-private (is-valid-tier (tier (string-ascii 6)))
    (is-some (map-get? subscription-tiers tier)))

(define-private (is-valid-subscriber (subscriber principal))
    (and 
        (not (is-eq subscriber contract-owner))
        (not (is-eq subscriber tx-sender))))

(define-private (is-subscription-active (subscriber principal))
    (begin 
        (asserts! (is-valid-subscriber subscriber) false)
        (let ((sub (unwrap! (map-get? subscriptions subscriber) false)))
            (and 
                (get active sub)
                (> (get expiration sub) block-height)))))

(define-private (auto-renew-subscription (subscriber principal))
    (begin 
        (asserts! (is-valid-subscriber subscriber) (err err-invalid-subscriber))
        (let ((current-sub (unwrap! (map-get? subscriptions subscriber) (err err-auto-renew-failed)))
              (tier-info (unwrap! (map-get? subscription-tiers (get tier current-sub)) (err err-invalid-tier))))
            
            (if (get auto-renew current-sub)
                (begin
                    (asserts! (> (get duration tier-info) u0) (err err-invalid-tier))
                    (map-set subscriptions subscriber
                        {tier: (get tier current-sub),
                         expiration: (+ block-height (get duration tier-info)),
                         active: true,
                         auto-renew: true,
                         trial-used: (get trial-used current-sub),
                         trial-expiration: (get trial-expiration current-sub)})
                    (ok true))
                (err err-auto-renew-failed)))))

;; Public Functions
(define-public (initialize)
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (not (var-get contract-initialized)) err-already-initialized)
        
        ;; Initialize subscription tiers with trial durations
        (map-set subscription-tiers "bronze"
            {price: u100,
             duration: u4320, ;; 30 days in blocks (assuming 10min block time)
             trial-duration: u288, ;; 2 days trial
             benefits: "Basic access to content"})
        
        (map-set subscription-tiers "silver"
            {price: u250,
             duration: u4320,
             trial-duration: u288,
             benefits: "Premium access + exclusive content"})
        
        (map-set subscription-tiers "gold"
            {price: u500,
             duration: u4320,
             trial-duration: u288,
             benefits: "All access + early releases"})
        
        (var-set contract-initialized true)
        (ok true)))

(define-public (subscribe (tier (string-ascii 6)) (enable-auto-renew (optional bool)))
    (begin 
        ;; Validate tier
        (asserts! (is-valid-tier tier) err-invalid-tier)
        
        (let ((tier-info (unwrap! (map-get? subscription-tiers tier) err-invalid-tier))
              (price (get price tier-info))
              (duration (get duration tier-info))
              (trial-duration (get trial-duration tier-info))
              (current-sub (map-get? subscriptions tx-sender)))
            
            ;; Check if subscriber has not used trial before
            (asserts! 
                (or 
                    (is-none current-sub) 
                    (not (get trial-used (unwrap-panic current-sub))))
                err-trial-expired)
            
            ;; Validate block height calculations
            (asserts! (> duration u0) err-invalid-tier)
            (asserts! (> trial-duration u0) err-invalid-tier)
            
            ;; Create or update subscription
            (map-set subscriptions tx-sender
                {tier: tier,
                 expiration: (+ block-height 
                                (if (is-none current-sub) 
                                    trial-duration 
                                    duration)),
                 active: true,
                 auto-renew: (default-to false enable-auto-renew),
                 trial-used: (is-none current-sub),
                 trial-expiration: (+ block-height trial-duration)})
            
            (ok true))))

(define-public (renew-subscription)
    (let ((current-sub (unwrap! (map-get? subscriptions tx-sender) err-unauthorized))
          (tier-info (unwrap! (map-get? subscription-tiers (get tier current-sub)) err-invalid-tier)))
        
        (map-set subscriptions tx-sender
            {tier: (get tier current-sub),
             expiration: (+ block-height (get duration tier-info)),
             active: true,
             auto-renew: (get auto-renew current-sub),
             trial-used: (get trial-used current-sub),
             trial-expiration: (get trial-expiration current-sub)})
        
        (ok true)))

(define-public (cancel-subscription)
    (let ((current-sub (unwrap! (map-get? subscriptions tx-sender) err-unauthorized)))
        (map-set subscriptions tx-sender
            {tier: (get tier current-sub),
             expiration: block-height,
             active: false,
             auto-renew: false,
             trial-used: (get trial-used current-sub),
             trial-expiration: (get trial-expiration current-sub)})
        
        (ok true)))

(define-public (toggle-auto-renew (enable bool))
    (let ((current-sub (unwrap! (map-get? subscriptions tx-sender) err-unauthorized)))
        (map-set subscriptions tx-sender
            {tier: (get tier current-sub),
             expiration: (get expiration current-sub),
             active: (get active current-sub),
             auto-renew: enable,
             trial-used: (get trial-used current-sub),
             trial-expiration: (get trial-expiration current-sub)})
        
        (ok true)))

(define-public (process-auto-renewals (subscribers (list 100 principal)))
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (let ((renewal-results (map auto-renew-subscription subscribers)))
            (ok true))))

(define-public (check-subscription (subscriber principal))
    (begin
        (asserts! (is-valid-subscriber subscriber) (err err-invalid-subscriber))
        (ok (is-subscription-active subscriber))))

(define-read-only (get-subscription-info (subscriber principal))
    (map-get? subscriptions subscriber))

(define-read-only (get-tier-info (tier (string-ascii 6)))
    (map-get? subscription-tiers tier))

;; Contract initialization check
(asserts! (is-contract-owner) err-unauthorized)