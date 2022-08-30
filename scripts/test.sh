# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

## Run tests
## Util
daml test --project-root ../package/test/daml/Daml.Finance.Test.Util
## Date Utils
daml test --project-root ../package/test/daml/Daml.Finance.Util.Test
## Core
daml test --project-root ../package/test/daml/Daml.Finance.Holding.Test
daml test --project-root ../package/test/daml/Daml.Finance.Settlement.Test
daml test --project-root ../package/test/daml/Daml.Finance.RefData.Test
## Extensions
daml test --project-root ../package/test/daml/Daml.Finance.Instrument.Generic.Test
daml test --project-root ../package/test/daml/Daml.Finance.Instrument.Bond.Test
daml test --project-root ../package/test/daml/Daml.Finance.Instrument.Equity.Test
