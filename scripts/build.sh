#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Build Interfaces
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Common daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Asset daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Settlement daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Lifecycle daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Derivative daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Interface.Equity daml build

# Build Implementations
DAML_PROJECT=../package/main/daml/Daml.Finance.Common daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Asset daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Settlement daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Lifecycle daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.RefData daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Derivative daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Equity daml build
DAML_PROJECT=../package/main/daml/Daml.Finance.Bond daml build

if [[ -d ../.dars ]]; then
  rm -r ../.dars
fi

# Copy all package dars into a dedicated folder
mkdir ../.dars
cp ../package/main/daml/*/.daml/dist/* ../.dars/

DAML_PROJECT=../package/test/daml/Daml.Finance.Test.Util daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Common.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Asset.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Settlement.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Derivative.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Equity.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.Bond.Test daml build
DAML_PROJECT=../package/test/daml/Daml.Finance.RefData.Test daml build
