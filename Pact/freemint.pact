(defun all-ids ()
        @doc "Returns all the ids"
        (keys wl-free-nft)

        (defun get-latest-wlfree-nft-to-mint-data ()
               (let
                   (
                       (minted-count (get-count FREE_NFTS_MINTED_COUNT_KEY))
                       (created-to-mint-count NFTS_TO_MINT_COUNT)
                   )
                   (enforce (< 0.0 created-to-mint-count) "No nfts have been put up for mint")
                   (enforce
                       (< minted-count created-to-mint-count)
                        "All nfts put up for mint have already been minted, please check later"
                   )
                   (let
                       (
                         (id (int-to-str 10 (+ (floor minted-count) 1)))
                       )
                       id
                   )
               )
           )

(defun mint-nft-free (owner:string amount:integer)
        @doc "Mints an NFT from WL qty 5 free"
        (require-capability (PRIVATE))
        (require-capability (ACCOUNT_GUARD owner))
        (let (
                (id (get-latest-wlfree-nft-to-mint-data) )
            )
            (insert wl-free-nft id {
                "id": id,
                "item": { "edition": id },
                "date-minted": (at "block-time" (chain-data)),
                "owner": owner
            })
        )
        (increase-count FREE_NFTS_MINTED_COUNT_KEY 1.0)
    )
