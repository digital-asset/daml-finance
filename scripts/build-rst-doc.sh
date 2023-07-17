#!/usr/bin/env bash
# Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

project=$1
script_dir=$(cd "$(dirname $0)"; pwd -P)
docs_dir=${project}/docs
docs_build_dir=${project}/.docs
project_name=$(yq e '.name' ${project}/daml.yaml)

green='\033[0;32m'
boldGreen='\033[1;92m'
cyan='\033[0;36m'
boldWhite='\033[1;97m'
colour_off='\033[0m'

project_string="Building rst documentation for package - ${project_name}"
bar=""
for str in $(seq 1 `echo ${project_string} | wc -c`); do bar+="-"; done

echo -e "\n${boldGreen}${bar}${colour_off}"
echo -e "${boldGreen}${project_string}${colour_off}"
echo -e "${boldGreen}${bar}${colour_off}\n"

echo -e "${boldWhite}\nRetrieving and merging anchors...${colour_off}"
sdk_version=$(yq e '.sdk-version' daml.yaml)
daml_root=$(if [ -z ${DAML_HOME} ]; then echo ~/.daml; else echo ${DAML_HOME}; fi)
cp ${daml_root}/sdk/${sdk_version}/damlc/resources/daml-base-anchors.json ${docs_build_dir}/daml-base-anchors.json

echo -e "${boldWhite}\nGenerating Rst documentation for ${project_name}...${colour_off}"
DAML_PROJECT=${project} daml damlc docs \
  --output=${docs_build_dir}/${project_name} \
  --output-hoogle=${docs_build_dir}/daml-finance-hoogle.txt \
  --input-format=json \
  --format=Rst \
  --exclude-instances=HasField,HasImplementation,HasFromInterface,HasToInterface,HasInterfaceView,HasExercise,HasExerciseGuarded,HasFromAnyChoice,HasToAnyChoice \
  --drop-orphan-instances \
  --template=${docs_dir}/base-rst-template.rst \
  --index-template=${docs_dir}/base-rst-index-template.rst \
  --hoogle-template=${docs_dir}/base-hoogle-template.txt \
  --base-url=https://docs.daml.com/daml-finance/reference/code-documentation/daml-finance-rst \
  --input-anchor=${docs_build_dir}/daml-base-anchors.json \
  --output-anchor=${docs_build_dir}/${project_name}-anchors.json \
  ${docs_build_dir}/${project_name}-doc.json
echo -e "\n${cyan}Successfully generated Rst documentation for ${project_name}.${colour_off}"
