#!/bin/sh
set -xe
cabal build diff-loc --flag="+test"
cabal-docspec diff-loc \
  --check-properties \
  --property-variables "d i j r s t u v w x y z" \
  --timeout 20
