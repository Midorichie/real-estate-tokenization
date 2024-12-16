;; Real Estate Tokenization Platform
;; Version: 1.1.0

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-price (err u104))

;; Data Variables
(define-data-var platform-status bool true)
(define-data-var dividend-cycle uint u0)

;; Data Maps
(define-map properties
    { property-id: uint }
    {
        owner: principal,
        total-tokens: uint,
        price-per-token: uint,
        available-tokens: uint,
        property-uri: (string-ascii 256),
        is-active: bool,
        accumulated-rent: uint,
        last-dividend-cycle: uint
    }
)

(define-map token-holdings
    { property-id: uint, owner: principal }
    { 
        token-count: uint,
        last-dividend-claim: uint
    }
)

(define-map market-orders
    { property-id: uint, seller: principal }
    {
        token-count: uint,
        price-per-token: uint,
        is-active: bool
    }
)

;; SFTs (Semi-Fungible Tokens)
(define-non-fungible-token property-token uint)

;; Dividend Distribution Functions
(define-public (deposit-rent (property-id uint) (amount uint))
    (let (
        (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set properties
            { property-id: property-id }
            (merge property {
                accumulated-rent: (+ (get accumulated-rent property) amount)
            })
        )
        (var-set dividend-cycle (+ (var-get dividend-cycle) u1))
        (ok true)
    ))
)

(define-public (claim-dividends (property-id uint))
    (let (
        (property (unwrap! (map-get? properties { property-id: property-id }) err-not-found))
        (holding (unwrap! (map-get? token-holdings 
            { property-id: property-id, owner: tx-sender }) err-not-found))
        (unclaimed-cycles (- (var-get dividend-cycle) (get last-dividend-claim holding)))
        (dividend-per-token (/ (get accumulated-rent property) (get total-tokens property)))
        (dividend-amount (* (get token-count holding) dividend-per-token unclaimed-cycles))
    )
    (begin
        (asserts! (> unclaimed-cycles u0) err-unauthorized)
        ;; Transfer dividend amount in STX or Bitcoin would go here
        (map-set token-holdings
            { property-id: property-id, owner: tx-sender }
            (merge holding {
                last-dividend-claim: (var-get dividend-cycle)
            })
        )
        (ok dividend-amount)
    ))
)

;; Secondary Market Functions
(define-public (create-sell-order (property-id uint) (token-count uint) (price-per-token uint))
    (let (
        (holding (unwrap! (map-get? token-holdings 
            { property-id: property-id, owner: tx-sender }) err-not-found))
    )
    (begin
        (asserts! (>= (get token-count holding) token-count) err-insufficient-balance)
        (asserts! (> price-per-token u0) err-invalid-price)
        (map-set market-orders
            { property-id: property-id, seller: tx-sender }
            {
                token-count: token-count,
                price-per-token: price-per-token,
                is-active: true
            }
        )
        (ok true)
    ))
)

(define-public (buy-from-market (property-id uint) (seller principal) (token-count uint))
    (let (
        (order (unwrap! (map-get? market-orders 
            { property-id: property-id, seller: seller }) err-not-found))
        (buyer-holding (default-to { token-count: u0, last-dividend-claim: (var-get dividend-cycle) }
            (map-get? token-holdings { property-id: property-id, owner: tx-sender })))
        (seller-holding (unwrap! (map-get? token-holdings 
            { property-id: property-id, owner: seller }) err-not-found))
    )
    (begin
        (asserts! (get is-active order) err-unauthorized)
        (asserts! (>= (get token-count order) token-count) err-insufficient-balance)
        ;; Payment logic would go here (using STX or xBTC)
        ;; Update buyer holdings
        (map-set token-holdings
            { property-id: property-id, owner: tx-sender }
            { 
                token-count: (+ token-count (get token-count buyer-holding)),
                last-dividend-claim: (var-get dividend-cycle)
            }
        )
        ;; Update seller holdings
        (map-set token-holdings
            { property-id: property-id, owner: seller }
            {
                token-count: (- (get token-count seller-holding) token-count),
                last-dividend-claim: (get last-dividend-claim seller-holding)
            }
        )
        ;; Update or close order
        (map-set market-orders
            { property-id: property-id, seller: seller }
            (merge order {
                token-count: (- (get token-count order) token-count),
                is-active: (> (- (get token-count order) token-count) u0)
            })
        )
        (ok true)
    ))
)

;; Read-Only Functions
(define-read-only (get-market-order (property-id uint) (seller principal))
    (map-get? market-orders { property-id: property-id, seller: seller })
)

(define-read-only (get-unclaimed-dividends (property-id uint) (owner principal))
    (let (
        (property (unwrap! (map-get? properties { property-id: property-id }) (ok u0)))
        (holding (unwrap! (map-get? token-holdings 
            { property-id: property-id, owner: owner }) (ok u0)))
        (unclaimed-cycles (- (var-get dividend-cycle) (get last-dividend-claim holding)))
        (dividend-per-token (/ (get accumulated-rent property) (get total-tokens property)))
    )
    (ok (* (get token-count holding) dividend-per-token unclaimed-cycles)))
)
