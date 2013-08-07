#!/bin/sh

set -e
gem build kropotkin.gemspec
if [ -n "$1" ]; then
    mv kropotkin*.gem $1
fi