#! /usr/bin/env python2.5
# -*- coding: utf-8 -*-
#
# Copyright (c) 2008 Ryan Kanno (ryankanno@localkinegrinds.com)
# License: GNU GPLv3
 
import urllib2
from BeautifulSoup import BeautifulSoup
 
import smtplib
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.Utils import COMMASPACE, formatdate
from email import Encoders
from tempfile import NamedTemporaryFile
import datetime
import cgi
import urlparse
 
from optparse import OptionParser
import sys, logging, os
 
__doc__ = """
 
This script retrieves a URL and sends its contents via email to 
a list of recipients.  Typically, this script is run from a cron
job that sends emails to a Gmail account to archive the contents
of a URL.
 
Mail can be sent via normal or authenticated SMTP.  Tested using 
Gmail SMTP (authenticated), Webfaction SMTP (authenticated), and
localhost (normal).
 
Example:
 
Sends the contents of http://www.espn.com to friend@domain.com using your Gmail settings
 
    python web2email.py -u gmail_username \
                        -p gmail_password \
                        -f gmail_username@gmail.com \
                        -r friend@domain.com http://www.espn.com
 
Sends the contents of http://www.espn.com to friend@domain.com using your Webfaction settings
 
    python web2email.py -u webfaction_username \
                        -p webfaction_password \
                        -f webfaction_account@webfaction_domain.com \
                        -s smtp.webfaction.com \
                        -r friend@domain.com http://www.espn.com
 
Sends the contents of http://www.espn.com to friend@domain.com using your local settings
 
    python web2email.py -f your_email@domain.com \
                        -s localhost \
                        --port 25 \
                        -r friend@domain.com http://www.espn.com
"""
 
__author__  = "ryankanno@localkinegrinds.com"
__url__     = "http://blog.localkinegrinds.com"
__version__ = "0.1"
 
USAGE = "usage: %prog [options] url" 
DESC  = __doc__.split('\n\n')[0]
 
def configure_logging(log_level, format='%(asctime)s %(levelname)s %(message)s'):
    logging.basicConfig(level=log_level, format=format)
 
def _validate_options_and_args(parser, options, args):
    logging.debug("Validating options and arguments.")
    if (len(args) != 1):
        parser.error("Incorrect number of arguments.  Script expects 1 (URL to backup), but received %i." % len(args))
        sys.exit(2) # Command line syntax error
    elif not options.recipients: 
        parser.error("You must include at least one recipient.")
        sys.exit(1) 
    elif (options.username and options.password is None) or (options.username is None and options.password is not None):
        parser.error("You must include both a username and password.")
        sys.exit(1) 
    elif not options.from_email:
        parser.error("You must include a valid from email address.")
        sys.exit(1) 
 
def getPage(url):
    logging.debug("Attempting to retrieve %s" % url)
    try:
        response = urllib2.urlopen(url)
        return response.read()
    except urllib2.HTTPError, e:
        logging.error("HTTPError (%s) occurred retrieving %s" % (e.code, url))
        sys.exit(1)
    except urllib2.URLError, e:
        logging.error("URLError (%s) occurred retrieving %s" % (e.reason, url))
        sys.exit(1)

def fixLinks(html, url):
    def appendUrl(links, link_attribute, url):
        for link in links:
            try:
                href = link[link_attribute]
                if href is not None and not "http://" in href:
                    link[link_attribute] = urlparse.urljoin(url, cgi.escape(href))
            except KeyError:
                pass

    soup = BeautifulSoup(unicode(html, errors="ignore"))
    appendUrl(soup('a'), 'href', url)
    appendUrl(soup('img'), 'src', url)
    return soup.prettify()

def mail(send_from, send_to, subject, text, content_type, files=[], server='localhost', port=25, username=None, password=None):
    def _auth(server, port, username, password):
        logging.debug("Attempting to send email via %s:%i using the following credentials (%s:%s)." % (server, port, username, password))
        smtp = smtplib.SMTP(server, port) 
        smtp.ehlo()
        smtp.starttls()
        smtp.ehlo()
        smtp.login(username, password)
        smtp.sendmail(username, send_to, msg.as_string())
        smtp.close()
 
    def _unauth(server, port):
        logging.debug("Attempting to send email via %s:%i" % (server, port))
        smtp = smtplib.SMTP(server, port)
        smtp.sendmail(send_from, send_to, msg.as_string())
        smtp.close()
 
    assert type(send_to)==list
 
    msg=MIMEMultipart()
    msg['From'] = send_from
    msg['To'] = COMMASPACE.join(send_to)
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = subject
 
    text = MIMEText(text)
    text.set_type(content_type)
    text.set_param('charset', 'UTF-8')
 
    msg.attach(text)
 
    for f in files:
        part = MIMEBase('application', "octet-stream")
        part.set_payload(open(f[0].name,"rb").read())
        Encoders.encode_base64(part)
        part.add_header('Content-Disposition', 'attachment; filename="%s"' % f[1])
        msg.attach(part)
 
    if not username and not password:
        _unauth(server, port)
    else:
        _auth(server, port, username, password) 
 
def main():
    parser = OptionParser(usage=USAGE, description=DESC)
 
    parser.add_option("-u", "--username", dest="username", metavar="USER", help="Username to SMTP server")
    parser.add_option("-p", "--password", dest="password", metavar="PWD", help="Password to SMTP server")
    parser.add_option("-s", "--server", dest="server", metavar="SERVER", help="SMTP server (Defaults to Gmail)")
    parser.add_option("--port", dest="port", metavar="PORT", type="int", help="SMTP server port (Defaults to Gmail)")
    parser.add_option("-f", "--from", dest="from_email", metavar="FROM", help="From address")
    parser.add_option("-r", "--recipient", action="append", dest="recipients", metavar="RCPT", type="string", help="Email recipient")
    parser.add_option('-a', '--attach', action='store_true', dest='as_attachment', metavar="ATTACH", help='As attachment')
    parser.add_option('-t', '--test', action="store_true", dest="test", metavar="TEST", help="Run tests")
    parser.add_option('-v', '--verbose', action='store_const', dest='log_level', const=logging.DEBUG, help='Verbose output')
    parser.set_defaults(server="smtp.gmail.com", port=587, test=False, log_level=logging.INFO)
    (options, args) = parser.parse_args()
 
    _validate_options_and_args(parser, options, args)
    configure_logging(options.log_level)
 
    if options.test:
        _test() # Too lazy to write a test for this script.  @TODO - use mocks 
 
    # Retrieve URL and return html
    html = getPage(args[0])

    # Fix links in URL
    html = fixLinks(html, args[0])

    files = []

    if options.as_attachment:
        f = NamedTemporaryFile(delete=False)
        f.write(html)
        f.close()
        name = os.path.basename(args[0])
        files.append((f, name))

    # Send mail with returned html as body 
    mail(options.from_email, options.recipients, 
         '%s @ %s' % (args[0], (datetime.datetime.now().strftime("%A %B %d %I:%M:%S %p %Y"))), 
         html, 'text/html', files=files,
         server=options.server, port=options.port, username=options.username, password=options.password)

    for f in files:
        os.unlink(f[0].name)
 
    # Return with appropriate exit code
    sys.exit(0)
 
def _test():
    import doctest
    doctest.testmod(sys.modules[__name__])
 
if __name__ == '__main__':
    main()
