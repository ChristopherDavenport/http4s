#!/usr/bin/env bash

. $TRAVIS_BUILD_DIR/bin/setup.sh

# Install hugo static site generator from GitHub releases page.
curl -s -L "https://github.com/spf13/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_Linux-64bit.tar.gz" | tar xzf -

pwd

ls -lah

mv "$TRAVIS_BUILD_DIR/hugo" "$HOME/bin/hugo"

sbt ";makeSite"
