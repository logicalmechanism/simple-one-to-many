cabal clean
cabal build -w ghc-8.10.4
cabal run group-payout
echo "DONE"