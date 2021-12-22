#!/usr/bin/bash
set -e

# Check if user gave args
if [ $# -eq 0 ]
  then
    echo "Provide A Payment Address."
    exit
fi

curl -H "Content-Type: application/json" -v -X POST \
    -d @data.json  \
    localhost:9080/api/contract/activate