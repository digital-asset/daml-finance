#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

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

# For each dependency, download and install
while IFS=" " read -r url out
do
  darWithoutVersion=$(echo $out | cut -d "/" -f 2 | cut -d "." -f 1)
  printf "Copying local %s\n" "$darWithoutVersion"
  cp ../../../.dars/"${darWithoutVersion}"*.dar .lib/"${darWithoutVersion}".dar
done < .lib/${version}.conf

echo "All dependencies successfully copied!"
