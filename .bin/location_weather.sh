#!/usr/bin/env bash
set -euo pipefail

export PATH=/usr/local/bin:$PATH

while IFS=, read -r city country
do
    weather=$(curl -s "wttr.in/$city,$country?format=%t+%c")
    echo "$weather"
done < /Users/gosha/.current_location.txt
