#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Checks if the generated docs are consistent with the source docs
# If they're not, automatically synchronizes them

set -euo pipefail

DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

DIR_TO_CHECK=$DIR/../generated
TEMP_DIR=$(mktemp -d)

cp -r $DIR_TO_CHECK $TEMP_DIR

$DIR/generate-docs.sh

echo "Comparing the docs, expecting no diff:"
diff -r $DIR_TO_CHECK $TEMP_DIR/generated # If there's any diff, the diff will return 1 and fail the script

echo "SUCCESS"
