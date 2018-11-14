#!/bin/bash
docker run \
    -v ~/.stack:/root/.stack \
    -v $(pwd):/root/work \
    eckyputrady/haskell-build-web:lts-10.3

cd scripts
cp -r ../dist ./dist
docker build -f scripts/Dockerfile -t eckyputrady/hauth:latest .
rm -rf ./dist