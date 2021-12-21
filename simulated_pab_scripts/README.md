Testing the pab goes like this:


Create some wallets and create the data. The example below uses a single payout pkh.
```bash
bash create_wallet.sh 

# Example Output
#
# "RoyaltyPayout"
# Wallet ID: "9e0e80b6f15a73e848b39b81bffb9118d4123304"
# Wallet PKH: "7776c2f312d04cb54cfba28c663d7b8ca63ab3f36065abb61dc29b72"
# {"cdtRoyaltyPKHs":[{"getPubKeyHash":"7776c2f312d04cb54cfba28c663d7b8ca63ab3f36065abb61dc29b72"}],"cdtPayouts":[4567896],"cdtProfit":1234567}
# Instance ID: 413ccce1-e036-4aef-8a3e-42af342439e8


DATA='{"cdtRoyaltyPKHs":[{"getPubKeyHash":"7776c2f312d04cb54cfba28c663d7b8ca63ab3f36065abb61dc29b72"}],"cdtPayouts":[4567896],"cdtProfit":1234567}'
```

Create a wallet that will be the smoosher.

```bash
bash create_wallet.sh
# Instance ID: 9fe27fb0-369e-46f7-b65e-74dafa5b1deb
```

Use the smoosher's wallet instance to call the smoosh endpoint.

```bash
bash smoosh_the_ball.sh 9fe27fb0-369e-46f7-b65e-74dafa5b1deb ${DATA}
```

Create a wallet that will be the exploder.

```bash
bash create_wallet.sh 
# Instance ID: 905d0dc0-d6e0-406e-a010-eeb6c228415c
```

Use the exploder's wallet instance to call the explode endpoint.

```bash
bash explode_the_ball.sh 905d0dc0-d6e0-406e-a010-eeb6c228415c ${DATA}
```

The smoosher pays the ADA for the ball to be smooshed but the exploder pays the smart contract fee. The final result is the first wallet being paid the amount defined in the data variable and the exploder getting paid the profit.