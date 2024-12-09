#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

root_dir=$(cd "$(dirname $0)"; cd ..; pwd -P)

# Remove docs/build directory if it exists
echo "Removing docs build directory if any"
if [ -d "${root_dir}/docs/build" ]; then
  rm -rf "${root_dir}/docs/build"
  echo "Removed ${root_dir}/docs/build"
else
  echo "${root_dir}/docs/build does not exist"
fi
