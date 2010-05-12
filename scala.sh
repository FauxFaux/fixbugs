#!/bin/sh

set -eu

scala -cp $(ant testcp | grep 'echo' | cut -d ' ' -f 7)
