#!/usr/bin/bash
set -e

CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="data/royalty_payout.plutus"

SCRIPT_ADDRESS=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
BUYER_ADDRESS=$(cat buyer-wallet/payment.addr)

echo
echo "Script Address:" $SCRIPT_ADDRESS
${cli} query utxo --address ${SCRIPT_ADDRESS} --testnet-magic 1097911063

echo
echo "Buyer Address:" $BUYER_ADDRESS
${cli} query utxo --address ${BUYER_ADDRESS} --testnet-magic 1097911063
