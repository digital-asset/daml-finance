import os
import re
import glob
from collections import deque

# This script sorts import statements in .daml files located in the 'src' directory.
# It organizes import items in the following order within each import line:
# 1. Items not starting with '(', sorted alphabetically with capital letters first.
# 2. Items starting with '(', kept in their original order and placed at the end.
# Usage: Run the script by calling 'python sort-daml-imports.py'.
# Note: It is recommended to backup your files or ensure version control is used before running this
# script, as it modifies the files in place.
def sort_imports(file):
    with open(file, 'r') as f:
        lines = f.readlines()

    first_import_line_index = next((i for i, line in enumerate(lines) if line.startswith('import')), None)
    last_import_line_index = next((i for i, line in enumerate(reversed(lines)) if line.startswith('import')), None)

    if first_import_line_index is not None and last_import_line_index is not None:
        last_import_line_index = len(lines) - 1 - last_import_line_index  # correct index since we used reversed list

        part1 = lines[:first_import_line_index]
        part2 = lines[first_import_line_index:last_import_line_index + 1]
        part3 = lines[last_import_line_index + 1:]

        # Adjusted sorting: prioritize lines not starting with '('
        sorted_imports = sorted(part2, key=lambda x: (x.strip().startswith('import ('), x.lower()))
        sorted_imports_final = []

        for import_line in sorted_imports:
            if '(' in import_line and ')' in import_line:
                sorted_import_line = sort_within_parentheses(import_line)
                sorted_imports_final.append(sorted_import_line)
            else:
                sorted_imports_final.append(import_line)

        sorted_file = part1 + sorted_imports_final + part3

        with open(file, 'w') as f:
            for line in sorted_file:
                f.write(line)

# Utility function
def sort_within_parentheses(import_line):
    stack = deque()
    fragments = []
    sorted_import_line = import_line

    for i, char in enumerate(import_line):
        if char == '(':
            stack.append(i)
        elif char == ')' and stack:
            start = stack.pop()
            if not stack:
                in_brackets = import_line[start + 1:i]
                in_brackets_split = [x.strip() for x in in_brackets.split(',')]
                # Separate items starting with '('
                special_items = [x for x in in_brackets_split if x.startswith('(')]
                # Sort other items, capital letters first
                regular_items = sorted([x for x in in_brackets_split if not x.startswith('(')],
                                       key=lambda x: (x[0].islower(), x))
                # Concatenate the sorted regular items with the special items
                sorted_in_brackets = ', '.join(regular_items + special_items)
                fragments.append((start, i, sorted_in_brackets))

    for start, end, fragment in reversed(fragments):
        sorted_import_line = sorted_import_line[:start + 1] + fragment + sorted_import_line[end:]

    return sorted_import_line

# Paths
current_file_absolute = os.path.abspath(__file__)
current_directory = os.path.dirname(current_file_absolute)
parent_directory = os.path.abspath(os.path.join(current_directory, os.pardir))
src_directory = os.path.join(parent_directory, 'src')

for file in glob.glob(os.path.join(src_directory, '**/*.daml'), recursive=True):
    sort_imports(file)
