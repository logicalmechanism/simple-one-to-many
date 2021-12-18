echo $data
definition=$(curl -s http://localhost:9080/api/contract/definitions | jq .[0].csrDefinition)

WALLET_1=$(curl -s -d '' http://localhost:9080/wallet/create | jq)
WALLET_ID_1=$(echo $WALLET_1 | jq .wiWallet.getWalletId)
WALLET_PKH_1=$(echo $WALLET_1 | jq .wiPubKeyHash.getPubKeyHash)
echo $WALLET_PKH_1

# Wallet 1
echo "instance call"
w1_instance=$(curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": '${definition}', "caWallet":{"getWalletId": '$WALLET_ID_1'}}' \
  http://localhost:9080/api/contract/activate | jq .unContractInstanceId)
w1_instance=$(sed -e 's/^"//' -e 's/"$//' <<<"$w1_instance")
echo $w1_instance


data='{"cdtRoyaltyPKHs":[{"getPubKeyHash":'$WALLET_PKH_1'}],"cdtPayouts":[4567896],"cdtProfit":1234567}'
echo $data

echo "endpoint calls"
curl -H "Content-Type: application/json" \
  --request POST \
  --data $data \
  http://localhost:9080/api/contract/instance/$w1_instance/endpoint/smoosh

echo -e "\nSmoosh Done"

curl -s http://localhost:9080/api/contract/instance/$w1_instance/status | jq .cicStatus
