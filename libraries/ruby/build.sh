#!/bin/sh

set -e
gem build kropotkin.gemspec
mv kropotkin*.gem $1
