# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

## Clean Core
# Interfaces
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Common
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Holding
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Instrument.Base
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Settlement
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Lifecycle
# Implementations
daml clean --project-root ../package/main/daml/Daml.Finance.Common
daml clean --project-root ../package/main/daml/Daml.Finance.Holding
daml clean --project-root ../package/main/daml/Daml.Finance.Settlement
daml clean --project-root ../package/main/daml/Daml.Finance.Lifecycle
daml clean --project-root ../package/main/daml/Daml.Finance.Instrument.Base
daml clean --project-root ../package/main/daml/Daml.Finance.RefData

## Clean Extensions
# Interfaces
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Instrument.Generic
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Instrument.Bond
daml clean --project-root ../package/main/daml/Daml.Finance.Interface.Instrument.Equity
# Implementations
daml clean --project-root ../package/main/daml/Daml.Finance.Instrument.Generic
daml clean --project-root ../package/main/daml/Daml.Finance.Instrument.Bond
daml clean --project-root ../package/main/daml/Daml.Finance.Instrument.Equity

## Clean Tests
# Util
daml clean --project-root ../package/test/daml/Daml.Finance.Test.Util
# Core
daml clean --project-root ../package/test/daml/Daml.Finance.Common.Test
daml clean --project-root ../package/test/daml/Daml.Finance.Holding.Test
daml clean --project-root ../package/test/daml/Daml.Finance.Settlement.Test
daml clean --project-root ../package/test/daml/Daml.Finance.RefData.Test
# Extensions
daml clean --project-root ../package/test/daml/Daml.Finance.Instrument.Generic.Test
daml clean --project-root ../package/test/daml/Daml.Finance.Instrument.Bond.Test
daml clean --project-root ../package/test/daml/Daml.Finance.Instrument.Equity.Test
