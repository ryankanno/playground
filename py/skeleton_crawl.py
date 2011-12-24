#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import platform
import logging
import sys
import os
import time
import string

# I love this to abstract out httplib, urllib, etc complexities
import mechanize

""" This module is just a template I use before performing crawling magic """

__all__     = ['main']
__author__  = ""
__url__     = ""
__version__ = ""
__license__ = ""

LOG_LEVEL = logging.DEBUG
LOG_FORMAT = '%(asctime)s %(levelname)s %(message)s'


# Timing decorator
def timing(func):
    def wrapper(*args, **kwargs):
        start  = time.clock() if 'Windows' == platform.system() else time.time()
        result = func(*args, **kwargs)
        end    = time.clock() if 'Windows' == platform.system() else time.time()
        logging.info("{0} took {1:.3g} ms".format(func.func_name, (end-start) * 1000.0))
        return result
    return wrapper


def init_argparser():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('url', help='url to crawl')
    parser.add_argument('-d', '--directory', default=os.getcwd(), help='directory to store crawl')
    parser.add_argument('-t', '--run-tests', action='store_true', help='run all tests')
    return parser


def create_filename(fname):
    valid_chars = "-_.() {0}{1}".format(string.ascii_letters, string.digits)
    return ''.join(c for c in fname if c in valid_chars)


def get_browser():
    """ Set policies, user agents, proxies, behaviors, etc. in here """
    return mechanize.Browser()


def crawl_url(browser, url):
    logging.debug("Crawling {0}".format(url))
    response = browser.open(url)
    return response.read()


def save_crawl(path_to_file, response):
    logging.debug("Saving to: {0}".format(path_to_file))

    dirname = os.path.dirname(path_to_file)
    
    if not os.path.exists(dirname):
        os.makedirs(dirname)

    with open(path_to_file, "wb") as f:
        f.write(response)


def do_crawl_son(args):
    brow = get_browser()
    resp = crawl_url(brow, args.url)
    save_crawl(os.path.join(args.directory, create_filename(args.url)), resp)


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
            do_crawl_son(args)
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
