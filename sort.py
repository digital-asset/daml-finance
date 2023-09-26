import os
import re
import glob
import logging
from collections import deque

logging.basicConfig(filename='sort_imports.log', level=logging.INFO,
                    format='%(asctime)s - %(message)s', datefmt='%d-%b-%y %H:%M:%S')


def sort_within_parentheses(import_line):
    # a deque to store our parentheses
    stack = deque()
    fragments = []
    sorted_import_line = import_line

    for i, char in enumerate(import_line):
        if char == '(':
            stack.append(i)
        elif char == ')' and stack:
            start = stack.pop()
            if not stack:
                # when we've found a matching pair of parentheses
                # extract the text within the parentheses
                in_brackets = import_line[start+1:i]
                # sort the import statements within the parentheses
                in_brackets_split = sorted([x.strip() for x in in_brackets.split(',')])
                sorted_in_brackets = ', '.join(in_brackets_split)
                fragments.append((start, i, sorted_in_brackets))

    # replace fragments in sorted_import_line
    for start, end, fragment in reversed(fragments):
        sorted_import_line = sorted_import_line[:start+1] + fragment + sorted_import_line[end:]

    return sorted_import_line


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

        sorted_imports = sorted(part2, key=lambda x: re.split(' as | ', x)[1].lower())
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

        logging.info(f"Sorted file: {file}")
        for line in sorted_imports_final:
            logging.info(line)


# path should be the path to the directory with the .daml files
path = './src'

for file in glob.glob(os.path.join(path, '**/*.daml'), recursive=True):
    sort_imports(file)

