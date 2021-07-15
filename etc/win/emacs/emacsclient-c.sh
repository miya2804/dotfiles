#!/bin/sh
emacsclient -c --display=$(ip route | awk '/^default/ {print $3; exit}'):0.0 $1
