#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

# Build theme CSS
cd docs/sphinx/theme && sass \
  -I bower_components_static/bourbon/dist \
  -I bower_components_static/neat/app/assets/stylesheets \
  -I bower_components_static/font-awesome/scss \
  -I bower_components_static/wyrm/sass \
  --style compressed \
  --sourcemap=none \
  --update sass:da_theme/static/css

# Grunt build
grunt build