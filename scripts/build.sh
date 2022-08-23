#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# TODO: Need to build this first as the bond interface package depends on the date type definitions.
# These should be moved into Interface.Common (see https://github.com/DACH-NY/daml-finance/issues/288)
DAML_PROJECT=../package/main/daml/Daml.Finance.Common daml build

## Build Core

# Interfaces
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Common daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Asset daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Instrument.Base daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Settlement daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Lifecycle daml build

# Implementations
DAML_PROJECT=../package/main/daml/Daml.Finance.Asset daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Settlement daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Lifecycle daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Instrument.Base daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.RefData daml build

## Build Extensions

# Interfaces
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Instrument.Generic daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Instrument.Bond daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Instrument.Equity daml build

# Implementations
DAML_PROJECT=../package/main/daml/Daml.Finance.Instrument.Generic daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Instrument.Bond daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Instrument.Equity daml build

# Copy package dars into a dedicated folder
if [[ -d ../.dars ]]; then
  rm -r ../.dars
fi
mkdir ../.dars
cp ../package/main/daml/*/.daml/dist/* ../.dars/

## Build Tests

# Core
DAML_PROJECT=../package/test/daml/Daml.Finance.Test.Util daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Common.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Asset.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Settlement.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.RefData.Test daml build

# Extensions
DAML_PROJECT=../package/test/daml/Daml.Finance.Instrument.Generic.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Instrument.Equity.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Instrument.Bond.Test daml build
