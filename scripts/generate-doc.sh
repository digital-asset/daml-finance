# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Test

daml damlc docs \
  --output=daml-finance-rst \
  --input-format=json \
  --format=Rst \
  --exclude-instances=HasField \
  --drop-orphan-instances \
  --template=base-rst-template.rst \
  --index-template=base-rst-index-template.rst \
  --base-url=https://docs.daml.com/daml/daml-finance \
  --input-anchor=../daml/bazel-bin/compiler/damlc/daml-base-anchors.json \
  .docs/daml-finance.json
  # --hoogle-template=$./ \
  # --output-hoogle=./ \
  # --output-anchor=./ \
  # --target=1.dev \
