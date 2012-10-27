#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import platform
import logging
import sys
import time

""" This module is just a template I use before performing magic """

__all__ = ['main']
__author__ = ""
__url__ = ""
__version__ = ""
__license__ = ""

LOG_LEVEL = logging.DEBUG
LOG_FORMAT = '%(asctime)s %(levelname)s %(message)s'


# Timing decorator
def timing(func):
    def wrapper(*args, **kwargs):
        start = time.clock() if 'Windows' == platform.system() \
            else time.time()
        result = func(*args, **kwargs)
        end = time.clock() if 'Windows' == platform.system() else time.time()
        logging.info("{0} took {1:.3g} ms".format(func.func_name,
                    (end - start) * 1000.0))
        return result
    return wrapper


def init_argparser():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('-t', '--run-tests', action='store_true',
                        help='run all tests')
    return parser


@timing
def do_work_son(args):
    pass


def main(argv=None):
    logging.basicConfig(level=LOG_LEVEL, format=LOG_FORMAT)

    if argv is None:
        argv = sys.argv

    parser = init_argparser()
    args = parser.parse_args(argv)

    try:
        if args.run_tests:
            _test()
        else:
            do_work_son(args)
    except Exception as e:
        logging.error("OMGWTFBBQ: {0}".format(e.args))
        sys.exit(1)

    # Yayyy-yah
    return 0


def _test():
    """ Do some testing, yo """
    import doctest
    doctest.testmod(sys.modules[__name__])


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

# vim: filetype=python
