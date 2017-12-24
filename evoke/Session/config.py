# -*- coding: utf-8 -*-

"""
config file for base.Session (used by User)
"""

from evoke.data.schema import *

class Session(Schema):
  "browser session"
  id=TAG,KEY
  user=INT,0
  ip=TAG, ''
  forwarded=TAG, ''
  ua=TAG, ''
  expires=DATE
  stage=TAG,''
  
  # TwistedSession
  lastModified = DATE
