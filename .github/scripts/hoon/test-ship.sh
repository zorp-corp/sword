#!/bin/bash

set -xeuo pipefail

LENS_PORT=$(grep 'loopback' $URBIT_PIER/.http.ports | awk -F ' ' '{print $1}')
lensdojo() {
  curl -s                                                         \
    --data '{"source":{"dojo":"'"$1"'"},"sink":{"stdout":null}}'  \
    "http://localhost:$LENS_PORT"
}

#  XX: redo when conn.c cli available; just runs tests whether they pass or fail
lensdojo "-test /=$DESK=/tests ~"
