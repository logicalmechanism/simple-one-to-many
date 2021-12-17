# One To Many

This attempts to solve the problem of handling group payments from another contract, allowing far more transactions to be validated on-chain without overloading a single contract.

A ball contains some amount of ADA that will be paid out to many addresses. If they wish, the group smooshing the ball can place an ADA prize for the wallet that explodes the ball. An exploding ball sends ADA out to their defined destinations and pays the exploder.

The goal is giving an incentive to the exploders to find a ball within the ecosystem. If the profit is greater than the fee then a exploder will gain ADA. Making the profit large enough to cover the fee becomes easy with full balls. A ball will max out at the protocol parameters which is about 16 addresses and amounts.

## Setting up


```
cd royalty-payout
cabal clean
cabal build -w ghc-8.10.4
cabal run royalty-payout
```

This will be integrated soon.
```
cabal run plutus-starter-pab
```

## Using

Trace test can be ran from the repl.

```
cabal repl royalty-payout.cabal
```

Try out these functions inside the repl.
```
smooshTheBall
smooshFail
explodeTheBall
```

Use the ```:reload``` call to rebuild inside the repl for quick testing.


## Flow

![alt text](ADABall.png)