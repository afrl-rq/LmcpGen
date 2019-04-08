#!/usr/bin/env python3

## ===============================================================================
## Authors: AFRL/RQQA
## Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
##
## Copyright (c) 2017 Government of the United State of America, as represented by
## the Secretary of the Air Force.  No copyright is claimed in the United States under
## Title 17, U.S. Code.  All Other Rights Reserved.
## ===============================================================================

## This file was auto-created by LmcpGen. Modifications will be overwritten.

import argparse
from pathlib import Path
import os
import random
import string
import sys

from lmcp import LMCPFactory

-<import_all_messages_and_enums>-

class Generator:

    def __init__(self, dir, seed, limit, checksum):
        self.dir = dir
        self.seed = seed
        self.limit = limit
        self.checksum = checksum
        self.rng = random.Random(self.seed)

    -<build_message_options>-
    -<build_reference_messages>-

    def write(self):
        print("seed={}".format(self.seed))

        # NOTE: roundtrip pack/unpack to ensure serialized text reflects underlying precision of types

        -<generate_reference_messages>-

def valid_directory(string):
    dir_ = Path(string)
    if dir_.is_dir():
        return dir_
    else:
        raise argparse.ArgumentTypeError("\'{}\' does not exist".format(string))

def to_bool(string):
    if string.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif string.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

def main():
    parser = argparse.ArgumentParser(description="Generates reference messages (xml and corresponding binary) for use by implementations to verify correctness of serialization/deserialization functions")
    parser.add_argument("-dir", help="path to root output directory (default=cwd)", type=valid_directory, default=os.getcwd())
    parser.add_argument("-seed", help="random seed to use for number generator (default=random)", type=int, default=int.from_bytes(os.urandom(8), byteorder="big"))
    parser.add_argument("-limit", help="limit variable length arrays, including strings, to specified length (default=32)", type=int, default=32)
    parser.add_argument("-checksum", help="compute checksum (default", type=to_bool, default=False)
    args = parser.parse_args()

    generator = Generator(args.dir.joinpath("reference"), args.seed, args.limit, args.checksum)
    generator.write()

    return 0


if __name__ == "__main__":
    sys.exit(main())