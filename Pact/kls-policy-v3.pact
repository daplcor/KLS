(namespace "free")

(define-keyset "free.kls-admin" (read-keyset "kls-admin"))

(module kls-policy-v3 GOVERNANCE

  @doc "Policy for fixed issuance with royalty and quoted sale in specified fungible."

  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard "free.kls-admin" )))

  (implements kip.token-policy-v1)
  (use kip.token-policy-v1 [token-info])
  
  ;  (use util.guards)

  (defcap MINT (account:string)
     (compose-capability (PRIVATE))
   )

   (defcap PRIVATE ()
     true
   )

  (defcap WHITELIST ()
    (compose-capability (GOVERNANCE))
    (compose-capability (PRIVATE))
  )

  ; ============================================
  ; ==            SCHEMA AND TABLES           ==
  ; ============================================

  (defschema policy-schema
    fungible:module{fungible-v2}
    creator:string
    creator-guard:guard
    mint-guard:guard
    max-supply:decimal
    min-amount:decimal
    royalty-rate:decimal
    transaction-rate:decimal
  )

  (defschema collection-schema
    royalty-receiver:string ;account which receives the royalty
    royalty-rate:decimal
    transaction-receiver:string
    transaction-rate:decimal
    total-supply:integer ;total supply of tokens that will ever exist
    provenance-hash:string ;sha256 of combined string
    tokens-list:[string] ;list of sha256 of the images that will ever exist
    creator:string
    max-per-user:integer ;maximum NFT a user can mint
    max-per-wh:integer ;maximum NFT a whitelisted user can mint
    max-per-txn:integer
    max-per-free:integer ;maximum free NFT mints a k: account has
    price-per-nft:decimal
    whitelist-price:decimal
    creator-guard:guard
    public-mint-time:time
    whitelist-mint-time:time
    mint-end-time:time
    pre-sale-users:[string]
    name:string
    category:string
    fungible:module{fungible-v2}
  )

  (defschema mint-schema
    tokens-list:[integer]
    current-length:integer
    status:string
    public-minted:decimal
  )

(defschema counter-schema
  count:integer
  )

  (defschema account-schema
    account:string
    minted:integer
  )

  (defschema whitelist-schema
    account:string
    guard:guard
    claimed:integer
  )

  (defschema traits-schema
    trait-type:string
    value:string
  )

  (defschema token-metadata
    name:string
    description:string
    image:string
    image-hash:string
    attributes:[object{traits-schema}]
  )

  (deftable policies:{policy-schema})
  (deftable collection-info:{collection-schema})
  (deftable mint-status:{mint-schema})
  (deftable counter:{counter-schema})
  (deftable account-details:{account-schema})
  (deftable whitelists:{whitelist-schema})
  (deftable traits:{traits-schema})
  (deftable metadata:{token-metadata})



  (defconst TOKEN_SPEC "token_spec"
    @doc "Payload field for token spec")

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defconst MINT_STATUS "mint-status")
  (defconst COLLECTION_INFO "collection-info")
  (defconst MINT_PAUSED "mint-paused")
  (defconst MINT_STARTED "mint-started")
  (defconst MINT_COMPLETED "mint-completed")

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes:{quote-schema})


  ; ============================================
  ; ==                SALE                    ==
  ; ============================================

 ; Dedicated section to launching sales on the token.  This can be called by
 ; anyone who wants to use this policy, we are happy to offer support if
 ; needed on our discord.  Happy hunting creators!





  (defun get-policy:object{policy-schema} (token:object{token-info})
    (read policies (at 'id token))
  )

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      royalty-payout:decimal
      transaction-payout:decimal
      creator:string
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defun enforce-ledger:bool ()
     (enforce-guard (marmalade.ledger.ledger-guard))
   )

   (defun get-details:object{collection-schema} ()
   (read collection-info COLLECTION_INFO)
  )

   (defun get-wl-limit (account:string)
   (at 'max-per-wh (get-details))
 )

   (defun enforce-whitelist:bool (account:string guard:guard)
    (let ((max-per-wh:integer (get-wl-limit account)))
      (with-read whitelists account{
       'guard:= g,
       'claimed:= claimed
      }
       (enforce (= g guard) "Guards doesn't match.")
       (enforce (< claimed  max-per-wh) (format "You can Mint only {} tokens during whitelist" [max-per-wh]))
     )
    )
   )

   (defun get-account-minted:integer (account:string)
   (with-default-read account-details account
     {"minted": 0}
     {"minted":= minted}
   minted
   )
 )

  ;   (defun enforce-reserved (account:string)
  ;     (let ( (minted:integer (get-account-minted account))
  ;            (reserved:integer (get-reserved account)))
  ;       (enforce (< minted reserved) "You have already claimed the reserved tokens")
  ;     )
  ;   )

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string
      guard:guard
      amount:decimal
    )
    (enforce-ledger)
    (bind (get-policy token)
      { 'mint-guard:=mint-guard:guard
      , 'min-amount:=min-amount:decimal
      , 'max-supply:=max-supply:decimal
      }
      (enforce-guard mint-guard)
      (enforce (>= amount min-amount) "mint amount < min-amount")
      (enforce (<= (+ amount (at 'supply token)) max-supply) "Exceeds max supply")
  ))

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce-ledger)
    (enforce false "Burn prohibited")
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-ledger)
    (let* ( (spec:object{policy-schema} (read-msg TOKEN_SPEC))
            (fungible:module{fungible-v2} (at 'fungible spec))
            (creator:string (at 'creator spec))
            (creator-guard:guard (at 'creator-guard spec))
            (mint-guard:guard (at 'mint-guard spec))
            (max-supply:decimal (at 'max-supply spec))
            (min-amount:decimal (at 'min-amount spec))
            (royalty-rate:decimal (at 'royalty-rate spec))
            (transaction-rate:decimal (at 'transaction-rate spec))
            (creator-details:object (fungible::details creator ))
            )
      (enforce (>= min-amount 0.0) "Invalid min-amount")
      (enforce (>= max-supply 0.0) "Invalid max-supply")
      (enforce (=
        (at 'guard creator-details) creator-guard)
        "Creator guard does not match")
      (enforce (and
        (>= royalty-rate 0.0) (<= royalty-rate 1.0))
        "Invalid royalty rate")
      (insert policies (at 'id token)
        { 'fungible: fungible
        , 'creator: creator
        , 'creator-guard: creator-guard
        , 'mint-guard: mint-guard
        , 'max-supply: max-supply
        , 'min-amount: min-amount
        , 'royalty-rate: royalty-rate
        , 'transaction-rate: transaction-rate}))
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string
      amount:decimal
      sale-id:string
    )
    @doc "Capture quote spec for SALE of TOKEN from message"
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
       ,'royalty-rate:= royalty-rate:decimal
       ,'transaction-rate:= transaction-rate
       ,'creator:= creator:string
      }
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price))
            (royalty-payout:decimal
               (floor (* sale-price (+ royalty-rate transaction-rate)) (fungible::precision))) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price royalty-payout transaction-rate creator spec)))
      true
  )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      , 'transaction-rate:= transaction-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal
                    (floor (* sale-price (+ royalty-rate transaction-rate)) (fungible::precision)))
                 (payout:decimal (- sale-price (+ royalty-payout transaction-payout)) )
            (if
              (> royalty-payout 0.0)
              (fungible::transfer buyer creator royalty-payout transaction-payout)
              "No royalty")
               (fungible::transfer buyer recipient payout))
            (update policies qtoken {
              "owner": buyer
              }))
            true
          )
   )
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string )
    (enforce-ledger)
    (enforce-sale-pact sale-id)
    (bind (get-policy token)
      { 'fungible := fungible:module{fungible-v2}
      , 'creator:= creator:string
      , 'royalty-rate:= royalty-rate:decimal
      }
      (with-read quotes sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
        (enforce (= qtoken (at 'id token)) "incorrect sale token")
        (bind spec
          { 'price := price:decimal
          , 'recipient := recipient:string
          }
          (let* ((sale-price:decimal (* amount price))
                 (royalty-payout:decimal
                  (floor (* sale-price royalty-rate) (fungible::precision)))
               (payout:decimal (- sale-price royalty-payout)) )
          (if
            (> royalty-payout 0.0)
            (fungible::transfer buyer creator royalty-payout)
            "No royalty")
            (fungible::transfer buyer recipient payout))
            (update policies qtoken {
              "owner": buyer
              }))
            true
      )
    )
  )

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal )
    (enforce-ledger)
    (enforce false "Transfer prohibited")
  )
)

; ============================================
; ==               Free Mint                ==
; ============================================


; Please update the keys and logic for the counter, should be able to
; enforce count <= the amount set on account name.  if count exceeds,
; deny the transaction.


; (defun all-ids ()
;         @doc "Returns all the ids"
;         (keys wl-free-nft)
;
;         (defun get-latest-wlfree-nft-to-mint-data ()
;                (let
;                    (
;                        (minted-count (get-count FREE_NFTS_MINTED_COUNT_KEY))
;                        (created-to-mint-count NFTS_TO_MINT_COUNT)
;                    )
;                    (enforce (< 0.0 created-to-mint-count) "No nfts have been put up for mint")
;                    (enforce
;                        (< minted-count created-to-mint-count)
;                         "All nfts put up for mint have already been minted, please check later"
;                    )
;                    (let
;                        (
;                          (id (int-to-str 10 (+ (floor minted-count) 1)))
;                        )
;                        id
;                    )
;                )
;            )
;
; (defun mint-nft-free (owner:string amount:integer)
;         @doc "Mints an NFT from WL qty 5 free"
;         (require-capability (PRIVATE))
;         (require-capability (ACCOUNT_GUARD owner))
;         (let (
;                 (id (get-latest-wlfree-nft-to-mint-data) )
;             )
;             (insert wl-free-nft id {
;                 "id": id,
;                 "item": { "edition": id },
;                 "date-minted": (at "block-time" (chain-data)),
;                 "owner": owner
;             })
;         )
;         (increase-count FREE_NFTS_MINTED_COUNT_KEY 1.0)
;     ))
)

(defun initdb:string ()
  @doc "Initializes the guards and creates the tables for the module"

  (with-capability (GOVERNANCE)

)
)
(if (read-msg 'upgrade )
  ["upgrade complete"]
  [ (create-table free.kls-policy-v3.quotes)
    (create-table free.kls-policy-v3.policies)
    (create-table free.kls-policy-v3.collection-info)
    (create-table free.kls-policy-v3.mint-status)
    (create-table free.kls-policy-v3.counter)
    (create-table free.kls-policy-v3.account-details)
    (create-table free.kls-policy-v3.whitelists)
    (create-table free.kls-policy-v3.traits)
    (create-table free.kls-policy-v3.metadata)
     ]
)
