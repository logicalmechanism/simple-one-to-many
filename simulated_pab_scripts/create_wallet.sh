#!/usr/bin/bash
set -e

DEF=$(curl -s http://localhost:9080/api/contract/definitions | jq .[0].csrDefinition)
echo $DEF

WALLET=$(curl -s -d '' http://localhost:9080/wallet/create | jq)
WALLET_ID=$(echo $WALLET | jq .wiWallet.getWalletId)
echo "Wallet ID:" $WALLET_ID
WALLET_PKH=$(echo $WALLET | jq .wiPubKeyHash.getPubKeyHash)
echo "Wallet PKH:" $WALLET_PKH

data='{"cdtRoyaltyPKHs":[{"getPubKeyHash":'$WALLET_PKH'}],"cdtPayouts":[4567896],"cdtProfit":1234567}'
echo $data

INSTANCE=$(curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": '${DEF}', "caWallet":{"getWalletId": '$WALLET_ID'}}' \
  http://localhost:9080/api/contract/activate | jq .unContractInstanceId)
INSTANCE=$(sed -e 's/^"//' -e 's/"$//' <<<"$INSTANCE")
echo "Instance ID:" $INSTANCE