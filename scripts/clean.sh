#!/bin/bash
#
# Removes all temporary files creates by the pirouette tests
#
# Usage:
#
#   ./scripts/clean
#
#
rm ./*.log
rm ./*.trees
rm ./*.xml
rm ./*.xml.state

cd vignettes || exit 42
(
  rm ./*.log
  rm ./*.trees
  rm ./*.xml
  rm ./*.xml.state
)

cd tests/testthat || exit 42
(
  rm ./*.log
  rm ./*.trees
  rm ./*.xml
  rm ./*.xml.state
)
