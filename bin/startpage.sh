#/usr/bin/env bash
set -euo pipefail

sudo /sbin/ifconfig lo0 alias 127.1.0.1

#exec python3 -m http.server --directory ~/.config/startpage --bind 127.1.0.1 80
