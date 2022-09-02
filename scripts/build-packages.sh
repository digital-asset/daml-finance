#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)
root_dir=$(cd ${script_dir}; cd ..; pwd -P)

## Build Core
# Interfaces
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Types
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Util
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Holding
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Base
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Settlement
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Lifecycle
# Implementations
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Util
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Holding
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Settlement
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Lifecycle
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Base
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.RefData

## Build Extensions
# Interfaces
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Generic
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Bond
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Equity
# Implementations
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Generic
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Bond
${script_dir}/build-package.sh ${root_dir}/package/main/daml/Daml.Finance.Instrument.Equity

# Copy package dars into a dedicated folder
if [[ -d ${root_dir}/.dars ]]; then
  rm -r ${root_dir}/.dars
fi
mkdir ${root_dir}/.dars
cp ${root_dir}/package/main/daml/*/.daml/dist/* ${root_dir}/.dars/

## Build Tests
# Util
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Test.Util
# Core
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Util.Test
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Holding.Test
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Settlement.Test
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.RefData.Test
# Extensions
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Instrument.Generic.Test
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Instrument.Bond.Test
${script_dir}/build-package.sh ${root_dir}/package/test/daml/Daml.Finance.Instrument.Equity.Test

echo -e "\nAll packages successfully built!"
