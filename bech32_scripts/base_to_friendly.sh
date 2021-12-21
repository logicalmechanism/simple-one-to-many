#!/usr/bin/bash
set -e

pkh=$(bash find_pubkeyhash.sh ${1})
echo $pkh
bash find_address.sh $pkh