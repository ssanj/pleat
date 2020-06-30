#!/bin/bash

set -Eeuo pipefail

PROG=$(basename "$0")

if [[ "$#" -lt 2 ]]; then
  echo "usage: $PROG REPO OS"
  echo "  REPO: name of repository"
  echo "  OS: name of OS (osx|linux)"
  echo "example: $PROG pleat osx"
  exit 1
fi

REPO=$1
OS=$2

if [[ "$OS" !=  'osx' ]] && [[ "$OS" != 'linux' ]]; then
  echo "OS must be one of 'osx' or 'linux'"
  exit 1
fi

VERSION=$(curl -s "https://api.github.com/repos/ssanj/$REPO/releases/latest" | jq '.tag_name' | sed s/\"//g)
echo "Downloading version: $VERSION"
FILE_NAME="$REPO-$VERSION-$OS.tar.gz"
DOWNLOAD_LOCATION="$HOME/Downloads/$FILE_NAME"
EXTRACT_LOCATION="$HOME/.local/bin/"
curl -L "https://github.com/ssanj/$REPO/releases/download/$VERSION/$FILE_NAME" -o "$DOWNLOAD_LOCATION"
gunzip -c "$DOWNLOAD_LOCATION" | tar -x -C "$EXTRACT_LOCATION" -
($REPO -v)