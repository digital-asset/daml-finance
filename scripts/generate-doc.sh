#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# This script should be run from the repo root directory.
# Example usage: ./scripts/generate-doc.sh ../daml/bazel-bin/compiler/damlc/daml-base-anchors.json

set -eu

# The anchors file is the `daml-base-anchors.json` file shipped with the SDK.
# It can also be found in ../daml/bazel-bin/compiler/damlc/daml-base-anchors.json after successfully building the daml repo
anchors_file=$1

echo "Generating documentation in json format."
make docjson

echo "Convert json documentation to rst."
daml damlc docs \
  --output=.docs/daml-finance-rst \
  --input-format=json \
  --format=Rst \
  --exclude-instances=HasField \
  --drop-orphan-instances \
  --template=.docs/base-rst-template.rst \
  --index-template=.docs/base-rst-index-template.rst \
  --base-url=https://docs.daml.com/daml/daml-finance \
  --input-anchor=${anchors_file} \
  .docs/daml-finance.json
  # --hoogle-template= \
  # --output-hoogle= \
  # --output-anchor= \
  # --target=1.dev \
