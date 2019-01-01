#!/usr/bin/env python3
# PROGRAMMER  : Sam Colwell
# FILE        : generate_glossary.py
# DATE        : 2019-01-01
# DESCRIPTION : Generate the glossary for the manual for Tali Forth 2
# from native_words.asm.
# Based on Scot W. Stevenson's generate_wordlist.py.
"""Creates an asciidoc formated glossary of words based on the header
comments in native_words.asm.
"""

import sys
import re
from enum import Enum

SOURCE = 'native_words.asm'
MARKER = '; ## '

# This runs as a state machine to capture the wordname, short 
# description, and the first comment block for each word.
class State(Enum):
    IDLE = 0
    HEADER = 1
    COMMENT = 2

# Regular expressions to capture the data in the header.
# Capture everything after the marker and first word.
first_line_re = re.compile(r"; ## \w+\s+(.*)$")
# Capture the name of the word and where it comes from
second_line_re = re.compile(r"; ## \"(\S+)\"\s+\w+\s+(.*)$")

def main():

    with open(SOURCE) as f:
        raw_list = f.readlines()

    # Set up dictionaries to store the data using the word name as 
    # the key.
    short_descr = {}
    source      = {}
    long_descr  = {}

    # Use the state machine to process the input.
    current_state = State.IDLE
    for line in raw_list:

        if current_state == State.IDLE:
            # In the idle state, we are on the lookout for the
            # beginning of a header.
            if line.startswith(MARKER):
                # We've found a header.  We don't know the word's
                # name yet, so just save this line.
                first_line = line
                current_state = State.HEADER
        elif current_state == State.HEADER:
            # We're in a header on the second line.
            # Determine the name, and then save the short description
            # and where it comes from
            match = second_line_re.match(line)
            if match:
                word_name = match.group(1)
                word_source = match.group(2)
                # DEBUG
                print('Found: '+word_name)
                # Try to get the description from the first line.
                descr_match = first_line_re.match(first_line)
                if descr_match:
                    # Save everything so far.
                    short_descr[word_name] = descr_match.group(1)
                    source[word_name] = word_source
                    # Look for the comment next.
                    current_state = State.COMMENT
                else:
                    # Something went wrong processing the first line.
                    print("Error determining name on line:")
                    print(first_line)
                    current_state = State.IDLE
            else:
                # Something went wrong on the second line.
                print("Error determining short description on line:")
                print(line)
        elif current_state == State.COMMENT:
            # Save the multi-line comments.
            line = line.strip()
            if line.startswith(';'):
                # It's a comment line.  Strip the semicolon and the
                # triple quotes.
                line = line.strip(';')
                line = line.strip()
                line = line.strip('"""')
                # Add this to any existing long description content.
                long_descr[word_name] = long_descr.get(word_name, '') \
                                        + line + "\n"
            else:
                # Not a comment line - back to the idle state.
                current_state = State.IDLE
    # All of the words have been read in.
    # Print them back out in ASCIIbetical order.
    for word_name in sorted(short_descr.keys()):
        print(word_name + short_descr[word_name])


if __name__ == '__main__':
    main()