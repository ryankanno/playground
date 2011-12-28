#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import logging
import argparse
import os

from utilities import get_followers

"""
Function determines all follower ids for an account.  Need to watch out who you
are requesting, this could put you over the ratelimit.  Please make sure to 
have a configuration file named twitter.config.json that contains the 
appropriate OAuth tokens.
"""

LOG_LEVEL  = logging.DEBUG
LOG_FORMAT = '%(asctime)s %(levelname)s %(message)s'
DEFAULT_TWITTER_CONFIG_FNAME=os.path.join(os.getcwd(), 'twitter.config.json')


def init_argparser():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('screen_name', help='all friend ids of screen name')
    parser.add_argument('-c', '--config', help='configuration file')
    return parser


def main(argv=None):
    logging.basicConfig(level=LOG_LEVEL, format=LOG_FORMAT)

    if argv is None:
        argv = sys.argv

    parser = init_argparser()
    args = parser.parse_args(argv)

    try:
        config_fname = args.config if args.config else DEFAULT_TWITTER_CONFIG_FNAME
        followers = get_followers(config_fname, args.screen_name)
        print("{0} has {1} followers".format(args.screen_name, len(followers)))
    except Exception as e:
        logging.error("OMGWTFBBQ: {0}".format(e.args))
        sys.exit(1)

    # Yayyy-yah
    return 0


if __name__ == "__main__":
    logging.basicConfig(level=LOG_LEVEL, format=LOG_FORMAT)
    sys.exit(main(sys.argv[1:]))

# vim: filetype=python
