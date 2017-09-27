#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Aditya Mangal'
SITENAME = u'Aditya Mangal'
SITEURL = ''

PATH = 'content'

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = u'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

# Blogroll
LINKS = (('About Me', 'http://adityamangal.com/about-me.html'),
         ('Contact', 'http://adityamangal.com/contact.html'),
        )

# Social widget
SOCIAL = (('github', 'https://github.com/adityamangal410'),
          ('linkedin', 'https://www.linkedin.com/in/adityamangal410/'),)

DEFAULT_PAGINATION = 8

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

THEME = 'Flex'

PLUGIN_PATHS = ['/Users/amangal/pelican-plugins/']
PLUGINS = ['sitemap']
