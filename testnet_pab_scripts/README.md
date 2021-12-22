Steps:

You will need ~6 terminals to run all the different components. Use the most recent version of PAB by fetching all the new tags.

```bash
cd plutus-apps
git fetch --all --recurse-submodules --tags
```

Build all the necessary components:

```bash
cd plutus-apps
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cabal build plutus-pab-examples plutus-chain-index cardano-node cardano-wallet
```

Start the testnet node locally, this may take a while to sync. Copy over an already sync db to speed up the process.

```bash
cd plutus-pab/test-node/
./start-testnet-node.sh
```

Run the cardano wallet backend

```bash
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-pab/test-node/
cabal exec -- cardano-wallet serve --testnet testnet/testnet-byron-genesis.json --node-socket testnet/node.sock
```

If this is the first time then a wallet needs to be created on the testnet. This is an actual testnet wallet.


Generate a seed phrase for the wallet.

```bash
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-pab/test-node/
cabal exec -- cardano-wallet recovery-phrase generate
```

Place into a file called testnet/restore-wallet.json.

```
# Example format
{ "name": "PAB testing wallet"
, "mnemonic_sentence": ["word1", "word2", ...]
, "passphrase": "pab123456789"
}
```

Load the wallet from the restore-wallet.json file. This can only be called once per run. Once a wallet is loaded it can be referenced by the wallet id.

```bash
loadwallet=$(curl -H "content-type: application/json" -XPOST \
  -d @testnet/restore-wallet.json \
  localhost:8090/v2/wallets)

export WALLET_ID=$(echo $loadwallet | jq .id)
echo $WALLET_ID
```

This wallet will need to sync with the chain. Look at the cardano-wallet terminal and check for the sync log.

Get an address for the wallet.

```bash
addr=$(curl -H "content-type: application/json" \
    -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq '.[0].id')
echo $addr
```

Show the current balance of the wallet.

```bash
curl -H "content-type: application/json" \
    -XGET localhost:8090/v2/wallets/$WALLET_ID | jq .balance.available
```

Open a new terminal and start the chain index.

```bash
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-pab/test-node/
cabal exec -- plutus-chain-index --config testnet/chain-index-config.json start-index
```

Open a new terminal and start the PAB

If it's the first time your running, you'll need to ask the PAB to make the database:

```bash

cabal exec -- plutus-pab-examples --config testnet/pab-config.yml migrate
```

Then, run the PAB

```bash
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
cd plutus-pab/test-node/
cabal exec -- plutus-pab-examples --config testnet/pab-config.yml webserver --passphrase pab123456789
```


Wait for all chain clients (cardano-node, cardano-wallet, plutus-pab-examples, plutus-chain-index) to fully synchronise with the testnet.

Open up another terminal and use it to check the sync status.

```bash
. /home/cardano/.nix-profile/etc/profile.d/nix.sh
nix-shell
curl -s localhost:9083/tip | jq '.tipSlot.getSlot'
```

Full sync time is a couple hours. It must be synced for this to work else things wont show up for a while. Once everything is synced then run the integration test to check if the full setup is working.

## The E2E integration test.

```bash
curl -H "Content-Type: application/json" -v -X POST -d \
  "{\"caID\":{\"tag\":\"IntegrationTest\"},\"caWallet\":{\"getWalletId\":\"$WALLET_ID\"}}" \
  localhost:9080/api/contract/activate
```


## One to Many Contract PAB Testnet

This is the attempt to run the contract using the pab on testnet. Only a single wallet is required to perform a E2E but the contract is designed to be general.