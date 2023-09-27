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
  #printf "Downloading: %s, to: %s\n" "$url" "$out"
  darWithoutVersion=$(echo $out | cut -d "/" -f 2 | cut -d "." -f 1)
  cp ../../../.dars/"${darWithoutVersion}"*.dar .lib/"${darWithoutVersion}".dar
  #dar = $(echo $out | cut -d " " -f 2)
  #printf "$dar"
  #curl -Lf# "${url}" -o ${out}
done < .lib/${version}.conf

echo "All dependencies successfully downloaded!"
