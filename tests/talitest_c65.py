#!/usr/bin/python3
"""Talitest_c65 starts the c65 65C02 emulator running Tali Forth 2 and
feeds it tests, saving the results.

RUNNING     : Run talitest_c65.py from the tests directory.

Results will be found in results.txt when finished.

PROGRAMMERS : Sam Colwell, Scot W. Stevenson, Patrick Surry
FILE        : talitest_c65.py

First version: 16. May 2018
This version: 06. Apr 2024
"""

import argparse
import sys

import subprocess


TESTER = 'tester.fs'
RESULTS = 'results.txt'
C65_LOCATION = '../c65/c65'
TALIFORTH_LOCATION = '../taliforth-c65.bin'
TALI_ERRORS = ['Undefined word',
               'Stack underflow',
               'ALLOT using all available memory',
               'Illegal SOURCE-ID during REFILL',
               'Interpreting a compile-only word',
               'DEFERed word not defined yet',
               'Division by zero',
               'Not in interpret mode',
               'Parsing failure',
               'No such xt found in Dictionary',
               'Digit larger than base',
               'QUIT could not get input (REFILL returned -1)',
               'Already in compile mode']

# Add name of file with test to the set of LEGAL_TESTS
LEGAL_TESTS = ['core_a', 'core_b', 'core_c', 'string', 'double',
               'facility', 'ed', 'asm', 'tali',
               'tools', 'block', 'search', 'user', 'cycles']
TESTLIST = ' '.join(["'"+str(t)+"' " for t in LEGAL_TESTS])

OUTPUT_HELP = 'Output File, default "'+RESULTS+'"'
TESTS_HELP = "Available tests: 'all' or one or more of "+TESTLIST

parser = argparse.ArgumentParser()
parser.add_argument('-b', '--beep', action='store_true',
                    help='Make a sound at end of testing', default=False)
parser.add_argument('-m', '--mute', action='store_true',
                    help='Only print errors and summary', default=False)
parser.add_argument('-o', '--output', dest='output',
                    help=OUTPUT_HELP, default=RESULTS)
parser.add_argument('-s', '--suppress_tester', action='store_true',
                    help='Suppress the output while the tester is loading', default=False)
parser.add_argument('-t', '--tests', nargs='+', type=str, default=['all'],
                    help=TESTS_HELP)
args = parser.parse_args()

# Make sure we were given a legal list of tests: Must be either 'all' or one or
# more of the legal tests
if (args.tests != ['all']) and (not set(args.tests).issubset(LEGAL_TESTS)):
    print('ERROR: Illegal test. Aborting.')
    sys.exit(1)

if args.tests == ['all']:
    args.tests = list(LEGAL_TESTS)

# Load the tester first.
with open(TESTER, 'r') as tester:
    # Create a string with all of the tests we will be running in it.
    test_string = tester.read()

# Load all of the tests selected from the command line.
for test in args.tests:

    # Determine the test file name.
    testfile = test + '.fs'

    with open(testfile, 'r') as infile:
        # Add a forth comment with the test file name.
        test_string = test_string +\
                      "\n ( Running test '{0}' from file '{1}' )\n".\
                      format(test, testfile)
        # Add the tests.
        test_string = test_string + infile.read()

# Have Tali2 quit at the end of all the tests.
test_string = test_string + "\nbye\n"

process = subprocess.Popen([C65_LOCATION, '-r', TALIFORTH_LOCATION], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
(out, err) = process.communicate(test_string.encode('ascii'))

# Log the results
with open(args.output, 'wb') as fout:
    fout.write(out)

# Walk through results and find stuff that went wrong
print()
print('='*80)
print('Summary for: ' + ' '.join(args.tests))

# Check to see if we crashed before reading all of the tests.
if f"bye c65:" not in out.decode('ascii'):
    print("Tali Forth 2 crashed before all tests completed\n")
else:
    print("Tali Forth 2 ran all tests requested")

# First, stuff that failed due to undefined words
undefined = []

with open(args.output, 'r') as rfile:

    for line in rfile:
        if 'undefined' in line:
            undefined.append(line)

# We shouldn't have any undefined words at all
if undefined:

    for line in undefined:
        print(line.strip())

# Second, stuff that failed the actual test
failed = []

with open(args.output, 'r') as rfile:

    for line in rfile:
        # Skip the message from compiling the test words
        if 'compiled' in line:
            continue

        if 'INCORRECT RESULT' in line:
            failed.append(line)

        if 'WRONG NUMBER OF RESULTS' in line:
            failed.append(line)

        for error_str in TALI_ERRORS:
            if error_str in line:
                failed.append(line)

if failed:

    for line in failed:
        print(line.strip())

# Sum it all up.
if (not undefined) and (not failed):
    print('All available tests passed')

# If we got here, the program itself ran fine one way or another
if args.beep:
    print('\a')

sys.exit(0)
