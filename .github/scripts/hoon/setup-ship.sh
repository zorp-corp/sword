#!/bin/bash

set -xeuo pipefail

DESK_DIR="$URBIT_PIER/$DESK"
DESK_LIB_DIR="$DESK_DIR/lib"
DESK_TST_DIR="$DESK_DIR/tests"

LENS_PORT=$(grep 'loopback' $URBIT_PIER/.http.ports | awk -F ' ' '{print $1}')
lensapp() {
  curl -s --max-time 10                                           \
    --data '{"source":{"dojo":"'"$2"'"},"sink":{"app":"'"$1"'"}}' \
    "http://localhost:$LENS_PORT"
}

lensdojo() {
  curl -s --max-time 10                                           \
    --data '{"source":{"dojo":"'"$1"'"},"sink":{"stdout":null}}'  \
    "http://localhost:$LENS_PORT"
}

#  XX: temporary; eventually should actually load files as pills
lensapp 'hood' "+hood/merge %$DESK our %base"

lensapp 'hood' "+hood/rm /=$DESK=/desk/bill"
lensapp 'hood' "+hood/rm /=$DESK=/sys/hoon/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/arvo/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/lull/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/zuse/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/ames/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/behn/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/clay/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/dill/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/eyre/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/gall/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/iris/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/jael/hoon"
lensapp 'hood' "+hood/rm /=$DESK=/sys/vane/khan/hoon"

lensapp 'hood' "+hood/mount %$DESK"

cp -rfL ./hoon/scaffolding/azimuth-pill.hoon  $DESK_DIR
cp -rfL ./hoon/scaffolding/baby.hoon          $DESK_DIR

cp -rfL ./hoon/scaffolding/cradle.hoon              $DESK_LIB_DIR
cp -rfL ./hoon/scaffolding/naive-cradle.hoon        $DESK_LIB_DIR
cp -rfL ./hoon/scaffolding/logs.jam                 $DESK_LIB_DIR
cp -rfL ./hoon/scaffolding/mainnet.azimuth-snapshot $DESK_LIB_DIR

lensapp 'hood' "+hood/commit %$DESK"

#  XX: No tests yet
#mkdir $DESK_TST_DIR
# cp -rfL ./hoon/scaffolding/tests/* $DESK_TST_DIR

#  XX: redo when conn.c cli available
check() {
  [ -z "$(lensdojo "-build-file /=$DESK=/$1/hoon" | grep 'thread failed')" ]
}

if check 'baby' && check 'azimuth-pill'; then
  echo "boot success"
else
  echo "boot failure"
  exit 1
fi
