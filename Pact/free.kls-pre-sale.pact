(namespace "free")

(define-keyset "free.kls-admin" (read-keyset "kls-admin"))

(module kls-pre-sale GOVERNANCE

  (use util.guards)

  (defschema reservation
    account:string
    guard:guard
    amount-kda:decimal
    amount-kls:integer)

  (defschema whitelist-schema
    accounts:[string]
  )

  (defschema sale
    status:string
    price:decimal)

  (deftable reservations:{reservation})
  (deftable whitelists:{whitelist-schema})
  (deftable sale-status:{sale})

  (defcap GOVERNANCE ()
    (enforce-guard
      (keyset-ref-guard "free.kls-admin")))

  (defcap OPS ()
    (enforce-guard
      (keyset-ref-guard "free.kls-ops")))

  (defcap RESERVE
    ( account:string
      amount-kda:decimal
      amount-kls:integer)
    "Reserve event for kls reservation"
    @event
    (enforce-guard (before-date END_TIME))
    (enforce-one "Sale Not Started"
      [(enforce-guard (at-after-date SALE_START_TIME))
      (enforce-guard (at-after-date WHITELIST_TIME))])
      (let* ( (total-kls-reserved:decimal (get-total-kls-reserved))
              (kls-reserved:integer (get-kls-reserved account))
              (sale-price:decimal (* amount-kls (get-sale-price))))
           (enforce
             (= amount-kda sale-price)
             (format "Invalid KDA/Kls amount {} KDA and {} Kls {}" [sale-price amount-kls amount-kda]))
           (enforce
            (<= (+ amount-kls total-kls-reserved) KLS_SALE_SUPPLY)
              (format "Reachecd maximum supply {} for private-sale" [KLS_SALE_SUPPLY]))
           (enforce
            (<= (+ amount-kls kls-reserved) KLS_PER_USER)
              (format "You can buy only {} tokens" [KLS_PER_USER]))
      )
    )

  (defconst KLS_BANK:string "k:7e3726d29367aff225eeb6d8a8d807de278d75c542f742d3bf1346d9fa74abd4")
  (defconst KLS_PER_USER:integer 5)
  (defconst KLS_SALE_SUPPLY:decimal 20.0)
  (defconst SALE_START_TIME:time (time "2022-03-25T16:00:00Z"))
  (defconst WHITELIST_TIME:time (time "2022-03-24T16:00:00Z"))
  (defconst END_TIME:time (time "2022-04-02T16:30:00Z"))
  (defconst SALE_STATUS:string 'sale-status)
  (defconst SALE_STARTED "sale-started")

  (defun init (accounts:[string])
    (with-capability (GOVERNANCE)
      (insert whitelists "" {
        'accounts: accounts
        })
      (insert sale-status SALE_STATUS {
        'status: SALE_STARTED,
        'price: 0.0
      })
    )
  )

  (defun enforce-whitelist (account)
    (let ( (accounts:[string] (at 'accounts (read whitelists ""))))
      (enforce (contains account accounts) "You are not whitelisted")
      (enforce-guard (at-after-date WHITELIST_TIME))
      (enforce-guard (at-before-date SALE_START_TIME))
    )
  )

  (defun reserve:string (account:string amount-kda:decimal amount-kls:integer)
    (enforce (<= 0 amount-kls) "amount-kls must atleast be 1")
    (with-capability (RESERVE account amount-kda amount-kls)
      (if (< (diff-time (at 'block-time (chain-data)) SALE_START_TIME) 0.0) (enforce-whitelist account) "Pre-sale for whitelists ended")
      (coin.transfer account KLS_BANK amount-kda)
      (let
        ( (g (at 'guard (coin.details account)))
          (kda-amount:decimal (+ amount-kda (get-amount-kda account)))
          (kls-amount:integer (+ amount-kls (get-kls-reserved account))))
        (write reservations account
          { "account"    : account
          , "amount-kda" : kda-amount
          , "amount-kls" : kls-amount
          , "guard"      : g
          })
        (format "{} reserved KLS with {} KDA" [account, amount-kda])
      )
    )
  )

  (defun update-sale-price:string (price:decimal)
    @doc   "Update sale price - Simplified oracle to handle on-chain reservation"
    (enforce (< 0.0 price) "price is not a positive number")
      (with-capability (GOVERNANCE)
        (with-read sale-status SALE_STATUS {
          "price":=oldPrice}
            (update sale-status SALE_STATUS {"price":price})
              (format "Kda/Usd sale price updated: old price {} | new price {}" [oldPrice, price])
        )
      )
  )

  (defun get-sale-price:decimal ()
    (at 'price (read sale-status SALE_STATUS))
  )

  (defun read-reservation (account:string)
    (read reservations account)
  )

  (defun get-accounts ()
    (keys reservations)
  )

  (defun get-total-kls-reserved:decimal ()
    (fold (+) 0.0 (map (get-kls-reserved) (get-accounts)))
  )

  (defun get-total-kda-reserved:decimal ()
    (fold (+) 0.0 (map (get-amount-kda) (get-accounts)))
  )

  (defun get-amount-kda:decimal (account:string)
    (with-default-read reservations account
      { 'amount-kda: 0.0 }
      { 'amount-kda:= amount }
      amount
    )
  )

  (defun get-kls-reserved:integer (account:string)
    (with-default-read reservations account
      { 'amount-kls: 0 }
      { 'amount-kls:= amount }
      amount
    )
  )

  (defun get-start-time:time ()
    SALE_START_TIME
  )

  (defun get-whitelist-time:time ()
    WHITELIST_TIME
  )

  (defun get-end-time:time ()
    END_TIME
  )

  (defun get-sale-supply:decimal ()
    KLS_SALE_SUPPLY
  )

  (defun get-kls-per-user:decimal ()
    KLS_PER_USER
  )

)
