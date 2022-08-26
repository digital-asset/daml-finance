#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

## Build Core
# Interfaces
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Types daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Util daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Holding daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Instrument.Base daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Settlement daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Lifecycle daml build
# Implementations
DAML_PROJECT=../package/main/daml/Daml.Finance.Util daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Holding daml build
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

## Copy package dars into a dedicated folder
if [[ -d ../.dars ]]; then
  rm -r ../.dars
fi
mkdir ../.dars
cp ../package/main/daml/*/.daml/dist/* ../.dars/

## Build Tests
# Util
DAML_PROJECT=../package/test/daml/Daml.Finance.Test.Util daml build
# Core
DAML_PROJECT=../package/test/daml/Daml.Finance.Util.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Holding.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Settlement.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.RefData.Test daml build
# Extensions
DAML_PROJECT=../package/test/daml/Daml.Finance.Instrument.Generic.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Instrument.Bond.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Instrument.Equity.Test daml build
