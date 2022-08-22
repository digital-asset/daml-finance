# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Test

daml test --project-root ../package/test/daml/Daml.Finance.Asset.Test
daml test --project-root ../package/test/daml/Daml.Finance.Instrument.Bond.Test
daml test --project-root ../package/test/daml/Daml.Finance.Common.Test
daml test --project-root ../package/test/daml/Daml.Finance.Generic.Test
daml test --project-root ../package/test/daml/Daml.Finance.Instrument.Equity.Test
daml test --project-root ../package/test/daml/Daml.Finance.Settlement.Test
daml test --project-root ../package/test/daml/Daml.Finance.RefData.Test
