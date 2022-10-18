# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

root_dir=$(cd "$(dirname $0)"; cd ..; pwd -P)

# Remove .lib directories in packages
echo "Removing .lib/ directories in all packages"
rm -r ${root_dir}/package/*/daml/*/.lib/ 1> /dev/null 2>&1

echo "Removing .dars/ directory"
rm -r ${root_dir}/.dars 1> /dev/null 2>&1

## Clean Core
# Contingent Claims
daml clean --project-root ${root_dir}/package/main/daml/ContingentClaims
# Interfaces
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Types
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Util
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Data
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Holding
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Account
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Base
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Claims
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Settlement
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Lifecycle
# Implementations
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Util
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Holding
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Account
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Settlement
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Lifecycle
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Data

## Clean Extensions
# Interfaces
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Token
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Generic
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Bond
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Equity
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Interface.Instrument.Swap
# Implementations
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Instrument.Token
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Instrument.Generic
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Instrument.Bond
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Instrument.Equity
daml clean --project-root ${root_dir}/package/main/daml/Daml.Finance.Instrument.Swap

## Clean Tests
# Util
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Test.Util
# Contingent Claims
daml clean --project-root ${root_dir}/package/test/daml/ContingentClaims.Test
# Core
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Util.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Holding.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Account.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Settlement.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Data.Test
# Extensions
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Instrument.Generic.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Instrument.Bond.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Instrument.Equity.Test
daml clean --project-root ${root_dir}/package/test/daml/Daml.Finance.Instrument.Swap.Test
