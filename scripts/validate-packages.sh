#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname $0)"; pwd -P)
root_dir=$(cd ${script_dir}; cd ..; pwd -P)

## Validate Core
# Contingent Claims
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/ContingentClaims.Core
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/ContingentClaims.Lifecycle
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/ContingentClaims.Valuation
# Interfaces
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Types
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Util
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Data
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Holding
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Account
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Base
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Claims
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Settlement
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Lifecycle
# Implementations
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Util
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Holding
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Account
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Settlement
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Lifecycle
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Claims
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Data

## Validate Extensions
# Interfaces
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Token
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Generic
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Bond
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Equity
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Swap
# Implementations
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Token
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Generic
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Bond
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Equity
${script_dir}/validate-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Swap

echo -e "\nAll packages are valid!"
