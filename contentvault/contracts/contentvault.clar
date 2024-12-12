;; ContentVault: Tokenized Subscription Service
;; Description: Smart contract for managing subscription-based access to digital content

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-initialized (err u101))
(define-constant err-not-initialized (err u102))
(define-constant err-subscription-expired (err u103))
(define-constant err-invalid-tier (err u104))
(define-constant err-unauthorized (err u105))

;; Data Variables
(define-data-var contract-initialized bool false)

;; Data Maps
(define-map subscriptions
    principal
    {tier: (string-ascii 6),
     expiration: uint,
     active: bool})

(define-map subscription-tiers
    (string-ascii 6)
    {price: uint,
     duration: uint,
     benefits: (string-ascii 50)})

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner))

(define-private (is-subscription-active (subscriber principal))
    (let ((sub (unwrap! (map-get? subscriptions subscriber) false)))
        (and 
            (get active sub)
            (> (get expiration sub) block-height))))

;; Public Functions
(define-public (initialize)
    (begin
        (asserts! (is-contract-owner) err-owner-only)
        (asserts! (not (var-get contract-initialized)) err-already-initialized)
        
        ;; Initialize subscription tiers
        (map-set subscription-tiers "bronze"
            {price: u100,
             duration: u4320, ;; 30 days in blocks (assuming 10min block time)
             benefits: "Basic access to content"})
        
        (map-set subscription-tiers "silver"
            {price: u250,
             duration: u4320,
             benefits: "Premium access + exclusive content"})
        
        (map-set subscription-tiers "gold"
            {price: u500,
             duration: u4320,
             benefits: "All access + early releases"})
        
        (var-set contract-initialized true)
        (ok true)))

(define-public (subscribe (tier (string-ascii 6)))
    (let ((tier-info (unwrap! (map-get? subscription-tiers tier) err-invalid-tier))
          (price (get price tier-info))
          (duration (get duration tier-info)))
        
        ;; Create or update subscription
        (map-set subscriptions tx-sender
            {tier: tier,
             expiration: (+ block-height duration),
             active: true})
        
        (ok true)))

(define-public (renew-subscription)
    (let ((current-sub (unwrap! (map-get? subscriptions tx-sender) err-unauthorized))
          (tier-info (unwrap! (map-get? subscription-tiers (get tier current-sub)) err-invalid-tier)))
        
        (map-set subscriptions tx-sender
            {tier: (get tier current-sub),
             expiration: (+ block-height (get duration tier-info)),
             active: true})
        
        (ok true)))

(define-public (cancel-subscription)
    (let ((current-sub (unwrap! (map-get? subscriptions tx-sender) err-unauthorized)))
        (map-set subscriptions tx-sender
            {tier: (get tier current-sub),
             expiration: block-height,
             active: false})
        
        (ok true)))

(define-public (check-subscription (subscriber principal))
    (ok (is-subscription-active subscriber)))

(define-read-only (get-subscription-info (subscriber principal))
    (map-get? subscriptions subscriber))

(define-read-only (get-tier-info (tier (string-ascii 6)))
    (map-get? subscription-tiers tier))

;; Contract initialization check
(asserts! (is-contract-owner) err-unauthorized)