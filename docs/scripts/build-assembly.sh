#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname $0)"; pwd -P)
docs_dir=$(cd ${script_dir}; cd ..; pwd -P)
root_dir=$(cd ${docs_dir}; cd ..; pwd -P)

# Remove any existing .assembly/ directories from docs
if [[ -d ${docs_dir}/.assembly ]]; then
  rm -rf ${docs_dir}/.assembly
fi
mkdir ${docs_dir}/.assembly

# Copy files into assembly file structure
cp -r ${docs_dir}/source/ ${docs_dir}/.assembly
cp -r ${root_dir}/src ${docs_dir}/.assembly

# Update the directory paths in the RST files as per the assembly structure
find ${docs_dir}/.assembly -type f -name '*.rst' -print0 | while IFS= read -r -d '' file
do
    # For BSD (ie, MacOS) and GNU compatibility (used in our Docker image)
    #  - specify a backup file as part of the 'in-place' flag which works for both distributions and then remove the generated backup file
    # Note - This should be resolved with DEV-ENV by depending on the GNU version
    sed -i.bck 's|.. literalinclude:: ../../|.. literalinclude:: |g' $file
    rm $file.bck
done
