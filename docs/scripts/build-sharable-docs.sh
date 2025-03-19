#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname $0)"; pwd -P)
docs_dir=$(cd ${script_dir}; cd ..; pwd -P)
root_dir=$(cd ${docs_dir}; cd ..; pwd -P)


if [[ -d ${docs_dir}/sharable ]]; then
  rm -rf ${docs_dir}/sharable
fi
mkdir ${docs_dir}/sharable
mkdir ${docs_dir}/sharable/src

cp -a ${docs_dir}/manually-written/* ${docs_dir}/sharable/
cp -a ${root_dir}/src/* ${docs_dir}/sharable/src/

mkdir -p ${docs_dir}/sharable/reference/code-documentation
cp -r ${docs_dir}/build/daml-finance-rst ${docs_dir}/sharable/reference/code-documentation/daml-finance-rst
cp ${docs_dir}/build/daml-finance-hoogle.txt ${docs_dir}/sharable/reference/daml-finance-hoogle.txt
