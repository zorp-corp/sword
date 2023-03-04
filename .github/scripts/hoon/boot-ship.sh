#!/bin/bash

set -xeuo pipefail

PILL_NAME='solid.pill'

curl -L $URBIT_URL | tar xzk --transform='s/.*/urbit/g'
curl -L $PILL_URL -o $PILL_NAME

./urbit                   \
  --bootstrap $PILL_NAME  \
  --local                 \
  --lite-boot             \
  --daemon                \
  --fake bus              \
  -c $URBIT_PIER

LENS_PORT=$(grep 'loopback' $URBIT_PIER/.http.ports | awk -F ' ' '{print $1}')
lensecho() {
  curl -s                                                             \
    --data '{"source":{"data":"'"$1"'"},"sink":{"stdout":null}}'      \
    "http://localhost:$LENS_PORT" | xargs printf %s | sed 's/\\n//g'
}

check() {
  [ "'3'" == "$(lensecho 3)" ]
}

if check; then
  echo "boot success"
else
  echo "boot failure"
  exit 1
fi
