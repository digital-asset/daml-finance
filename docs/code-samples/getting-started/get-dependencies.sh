#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0


set -eu

# Target Daml Finance version
version="0.1.3"

# Daml Finance dependent libraries
dependencies=( \
  "Daml.Finance.Holding" \
  "Daml.Finance.Instrument.Base" \
  "Daml.Finance.Interface.Holding" \
  "Daml.Finance.Interface.Instrument.Base" \
  "Daml.Finance.Interface.Settlement" \
  "Daml.Finance.Interface.Types" \
  "Daml.Finance.Settlement" \
)

for dependency in "${dependencies[@]}"; do

  echo "Processing dependency ${dependency}"

  package_name=`awk '{ l=tolower($0); gsub(/\./,"-",l); print l }' <<< ${dependency}`
  file_name="${package_name}-${version}.dar"

  if [[ -a lib/${file_name} ]]; then
    echo "Dependency ${file_name} already exists. Skipping..."
  else

    echo "Downloading ${file_name} from Github repository at https://github.com/digital-asset/daml-finance/releases/download/${dependency}/${version}/${file_name}."
    curl -Lf# https://github.com/digital-asset/daml-finance/releases/download/${dependency}/${version}/${file_name} -o lib/${file_name}

    echo -e "\nDependency ${file_name} downloaded successfully and saved to lib/${file_name}.\n"
  fi

done
