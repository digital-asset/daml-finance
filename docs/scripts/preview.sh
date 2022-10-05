#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

SCRIPT_DIR=$(dirname "$0")
cd $SCRIPT_DIR
BUILD_DIR=$(pwd)/build

cd $BUILD_DIR/html
python -m http.server 8000 --bind 127.0.0.1
