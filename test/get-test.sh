#!/bin/bash

set -e
REPO_URL="git@github.com:SingleStepTests/65x02.git"
FOLDER_PATH="nes6502"
BRANCH_NAME="main"

echo "Initializing download for '$FOLDER_PATH' from '$REPO_URL'..."
echo

echo "Initializing empty Git repository..."
git init

echo "Adding remote origin: '$REPO_URL'..."
git remote add -f origin "$REPO_URL"

echo "Enabling sparse checkout..."
git config core.sparseCheckout true

echo "Setting sparse checkout to download only '$FOLDER_PATH'..."
echo "$FOLDER_PATH/*" >>.git/info/sparse-checkout

echo "Performing shallow pull from branch '$BRANCH_NAME'..."
git pull --depth=1 origin "$BRANCH_NAME"

echo
echo "✅ Success!"
echo "The folder '$FOLDER_PATH' has been downloaded to ."
