#!/usr/bin/bash
set -e

curl -H "Content-Type: application/json" \
  --request POST \
  --data $2 \
  http://localhost:9080/api/contract/instance/$1/endpoint/smoosh

echo -e "\nSmoosh Done\n"

curl -s http://localhost:9080/api/contract/instance/$1/status | jq .cicStatus