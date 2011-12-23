#!/bin/sh
osascript -e 'tell application "terminal"' -e "do script \"cd $1\"" -e 'end tell'
