#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# This script is used to get the quickstarter/tutorial dependencies from locally compiled .dar files
# (instead of released .dar files).
# Instructions:
# 1. Build the Daml Finance .dar files locally in the usual way (e.g. using ``make ci-local``)
# 2. Navigate to a tutorial, e.g. ``cd docs/code-samples/payoff-modeling``
# 3. Run this script to install the dependencies: ``../scripts/get-local-dependencies.sh``

set -euo pipefail

# Create .lib directory
if [[ -d .lib ]]; then
  rm -r .lib
fi
mkdir .lib

# Get the dependency list
echo "Copying the list of dependencies"
version=$(grep '^version' daml.yaml | cut -d " " -f 2)
cp ../tutorials-config/${version}.conf .lib/${version}.conf

# For each dependency, install it (copy the .dar file to the tutorial .lib folder)
while IFS=" " read -r url out
do
  darWithoutVersion=$(echo $out | cut -d "/" -f 2 | cut -d "." -f 1)
  printf "Copying local %s\n" "$darWithoutVersion"
  cp ../../../.dars/"${darWithoutVersion}"*.dar .lib/"${darWithoutVersion}".dar
done < .lib/${version}.conf

echo "All dependencies successfully copied!"
