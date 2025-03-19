# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

import os
import sys

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), 'exts')))

def setup(sphinx):
    from pygments_daml_lexer import DamlLexer
    sphinx.add_lexer("daml", DamlLexer)

