
echo "endpoint calls"
curl -H "Content-Type: application/json" \
  --request POST \
  --data $2 \
  http://localhost:9080/api/contract/instance/$1/endpoint/explode

echo -e "\nExplode Done\n"

curl -s http://localhost:9080/api/contract/instance/$1/status | jq .cicStatus