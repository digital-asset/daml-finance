#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname $0)"; pwd -P)
docs_dir=$(cd ${script_dir}; cd ..; pwd -P)
root_dir=$(cd ${docs_dir}; cd ..; pwd -P)


if [[ -d ${docs_dir}/generated ]]; then
  rm -rf ${docs_dir}/generated
fi
mkdir ${docs_dir}/generated
mkdir ${docs_dir}/generated/src

cp -a ${docs_dir}/source/* ${docs_dir}/generated/
cp -a ${root_dir}/src/* ${docs_dir}/generated/src/

mkdir -p ${docs_dir}/generated/reference/code-documentation
cp -r ${docs_dir}/build/daml-finance-rst ${docs_dir}/generated/reference/code-documentation/daml-finance-rst
cp ${docs_dir}/build/daml-finance-hoogle.txt ${docs_dir}/generated/reference/daml-finance-hoogle.txt

# Make the generated files read-only
find ${docs_dir}/generated -type f -follow -exec chmod 0444 {} +
