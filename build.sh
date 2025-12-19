#!/usr/bin/env bash
./site build
mkdir -p _site/fonts
cp -r fonts/* _site/fonts/
echo "Fonts copied to _site/fonts/"
