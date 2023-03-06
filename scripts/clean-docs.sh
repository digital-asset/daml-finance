#!/usr/bin/env bash
# Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

root_dir=$(cd "$(dirname $0)"; cd ..; pwd -P)

# Remove docs/build directory
echo "Removing docs build directory"
if [[ -d ${root_dir}/docs/build ]]; then
  rm -rf ${root_dir}/docs/build
fi
