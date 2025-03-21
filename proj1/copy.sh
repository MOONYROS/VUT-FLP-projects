#!/bin/bash

if [ ! -f "flp-fun" ]; then
  echo "Binary file not found!"
  exit 1
fi

cp flp-fun test/public/

echo "Copying was successful."
