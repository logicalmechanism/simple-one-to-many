#!/usr/bin/bash
set -e

# Check if user gave args
if [ $# -eq 0 ]
  then
    echo "Provide A Payment Address."
    exit
fi
./bech32 addr_test <<< 60${1}
# ./bech32 addr <<< 61${1}