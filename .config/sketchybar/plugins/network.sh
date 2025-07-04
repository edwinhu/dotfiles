#!/usr/bin/env bash

UPDOWN=$(ifstat-legacy -i "en0" -b 0.1 1 | tail -n1)
DOWN=$(echo $UPDOWN | awk "{ print \$1 }" | cut -f1 -d ".")
UP=$(echo $UPDOWN | awk "{ print \$2 }" | cut -f1 -d ".")

DOWN_FORMAT=""
if [ "$DOWN" -gt "999" ]; then
  DOWN_FORMAT=$(echo $DOWN | awk '{ printf "%03.0f Mbps", $1 / 1000}')
elif [ "$DOWN" -gt "0" ]; then
  DOWN_FORMAT=$(echo $DOWN | awk '{ printf "%03.0f kbps", $1}')
else
  DOWN_FORMAT=$(echo "--- kbps")
fi

UP_FORMAT=""
if [ "$UP" -gt "999" ]; then
  UP_FORMAT=$(echo $UP | awk '{ printf "%03.0f Mbps", $1 / 1000}')
elif [ "$UP" -gt "0" ]; then
  UP_FORMAT=$(echo $UP | awk '{ printf "%03.0f kbps", $1}')
else
  UP_FORMAT=$(echo "--- kbps")
fi

sketchybar -m --set network_down label="$DOWN_FORMAT" icon.highlight=$(if [ "$DOWN" -gt "0" ]; then echo "on"; else echo "off"; fi) \
  --set network_up label="$UP_FORMAT" icon.highlight=$(if [ "$UP" -gt "0" ]; then echo "on"; else echo "off"; fi)
