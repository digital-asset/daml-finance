# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

import os

# Script to double check that each package daml.yaml file version match the expected versions
# (stated in e.g. the docs https://docs.daml.com/2.8.0/daml-finance/index.html).

def check(expected_versions):
  # Current script directory
  script_dir = os.path.dirname(os.path.realpath(__file__))
  package_dir = os.path.join(script_dir, '..', 'package', 'main', 'daml')

  # Variable to track if any mismatches are found
  mismatch_found = False

  for root, dirs, files in os.walk(package_dir):
    for file in files:
      if file == "daml.yaml":
        file_path = os.path.join(root, file)
        try:
          with open(file_path, 'r') as stream:
            package_name = ''
            version = ''
            for line in stream:
              if line.startswith('name:'):
                package_name = root.split(os.sep)[-1]
              elif line.startswith('version:'):
                version = line.split(':', 1)[1].strip()

            if package_name and version:
              expected_version = expected_versions.get(package_name)

              if expected_version is None:
                print(f"Package {package_name} not found in the expected versions list.")
                mismatch_found = True
              elif version != expected_version:
                print(f"Version mismatch for {package_name}. Found: {version}, Expected: {expected_version}")
                mismatch_found = True
            else:
              print(f"Package name or version not found in {file_path}")
              mismatch_found = True

        except Exception as exc:
          print(f"Error processing file {file_path}: {exc}")
          mismatch_found = True

  if not mismatch_found:
    print("All daml.yaml files have the expected versions.")

# Expected versions for each package
check({
    # Stable Packages
    "ContingentClaims.Core": "2.0.1",
    "ContingentClaims.Lifecycle": "2.0.1",
    "Daml.Finance.Account": "3.0.0",
    "Daml.Finance.Claims": "2.1.0",
    "Daml.Finance.Data": "3.0.0",
    "Daml.Finance.Holding": "3.0.0",
    "Daml.Finance.Instrument.Bond": "2.0.0",
    "Daml.Finance.Instrument.Generic": "3.0.0",
    "Daml.Finance.Instrument.Token": "3.0.0",
    "Daml.Finance.Interface.Account": "3.0.0",
    "Daml.Finance.Interface.Claims": "3.0.0",
    "Daml.Finance.Interface.Data": "3.1.0",
    "Daml.Finance.Interface.Holding": "3.0.0",
    "Daml.Finance.Interface.Instrument.Base": "3.0.0",
    "Daml.Finance.Interface.Instrument.Bond": "2.0.0",
    "Daml.Finance.Interface.Instrument.Generic": "3.0.0",
    "Daml.Finance.Interface.Instrument.Token": "3.0.0",
    "Daml.Finance.Interface.Instrument.Types": "1.0.0",
    "Daml.Finance.Interface.Lifecycle": "3.0.0",
    "Daml.Finance.Interface.Settlement": "3.0.0",
    "Daml.Finance.Interface.Types.Common": "2.0.0",
    "Daml.Finance.Interface.Types.Date": "2.1.0",
    "Daml.Finance.Interface.Util": "2.1.0",
    "Daml.Finance.Lifecycle": "3.0.0",
    "Daml.Finance.Settlement": "3.0.0",
    "Daml.Finance.Util": "3.1.0",
    # Early Access Packages
    "ContingentClaims.Valuation": "0.2.2",
    "Daml.Finance.Instrument.Equity": "0.4.0",
    "Daml.Finance.Instrument.Option": "0.3.0",
    "Daml.Finance.Instrument.StructuredProduct": "0.1.0",
    "Daml.Finance.Instrument.Swap": "0.4.0",
    "Daml.Finance.Interface.Instrument.Equity": "0.4.0",
    "Daml.Finance.Interface.Instrument.Option": "0.3.0",
    "Daml.Finance.Interface.Instrument.StructuredProduct": "0.1.0",
    "Daml.Finance.Interface.Instrument.Swap": "0.4.0"
})
