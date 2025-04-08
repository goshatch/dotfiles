#!/usr/bin/env bash

# Retrieves a geolocation based on the current IP address and saves to a
# temporary file, which later can be accessed by other scripts.
#
# Requires:
# - curl
# - gq
# - a ipinfo.io access token passed as parameter
#
# The API key for ipinfo.io is either passed as argument, or retrieved from
# the IPINFO_API_KEY environment variable.
#
# This script needs to be ran every time the network situation changes (a good
# indication you're in a new location!)
#
# In order for this to happen on a Mac, create
# $HOME/Library/LaunchAgents/networkchange.plist with these contents:
#
# <?xml version="1.0" encoding="UTF-8"?>
# <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" \
#   "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
# <plist version="1.0">
# <dict>
#   <key>Label</key>
#   <string>networkchange</string>
#   <key>LowPriorityIO</key>
#   <true/>
#   <key>ProgramArguments</key>
#   <array>
#     <string>/Users/gosha/.bin/geolocate.sh</string>
#     <string>(API KEY HERE)</string>
#   </array>
#   <key>WatchPaths</key>
#   <array>
#     <string>/private/var/run/resolv.conf</string>
#   </array>
#   <key>RunAtLoad</key>
#   <true/>
# </dict>
# </plist>
#
# Then enable the service:
# $ launchctl load networkchange.plist
# $ launchctl start networkchange
#
# Source: https://apple.stackexchange.com/a/232168

args=("$@")
OUTPUT_FILE=$HOME/.current_location.txt
API_KEY=${IPINFO_API_KEY:-${args[0]}}

if [[ -z "${API_KEY}" ]]; then
  >&2 echo "No API key specified."
  exit 1
fi

curl "https://ipinfo.io/?token=${API_KEY}" |
  jq '.city, .country' |
  while read -r CITY; do
    read -r COUNTRY
    echo "${CITY},${COUNTRY}" |
      tr -d \" > \
        ${OUTPUT_FILE}
  done
