#!/bin/bash
set -e # makes the script fail if any one of the below operation fails

### prepare distribution folders

rm -rf dist
mkdir dist

### build & test

echo "Build & test"
stack build --test --coverage
echo "Build & test finished with exit code $?"
cp -r $(stack path --local-install-root)/bin dist/bin

### copy non-hs resources

cp -r app dist/app
cp -r src dist/src
cd dist
find . -name "*.hs" -type f -delete
find . -type d -empty -delete
cd ..

### code quality tools

echo "Installing code quality tools"
stack install hlint weeder hpc-threshold

echo "Running HLint ..."
hlint .
echo "HLint finished with exit code $?"

echo "Running Weeder ..."
weeder .
echo "Weeder finished with exit code $?"

echo "Running hpc-threshold ..."
(stack hpc report --all 2>&1) | hpc-threshold
echo "hpc-threshold finished with exit code $?"

### report

echo "Build finished. see /dist."