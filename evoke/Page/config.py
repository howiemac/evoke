"""
config file for Page

"""

from evoke.data.schema import *

# the following are defaults and should not be removed - they are likely to be overridden in an app's config.py or config_site.py
ratings = True  # can pages be rated? valid values are False, True, and "admin" (only page admins see/set ratings)
attribution = "full"  # are author's names credited in page headers and listings? Can be "full" (headers and lists), "minimal" (page headers only), or False
show_time = True  # is the date/time of items shown?
order_by = 'latest'  # default page listing order
thumb_size = 120  # image thumbnail size (width)  - this is storage size, they may be displayed smaller using CSS

# flat_files=False #set this to True if you wish posting of a page to publish it as flat html - NOT YET IMPLEMENTED
#                 #  - then urls in the form domain/123 will be served to a guest user as flat files

# the following are overrides of config_base parameters
permits = {
    'master': ['be'],
    'user': ['edit'],
    'page': ['create', 'edit', 'admin']
}  #basis of permit system
default_permits = {
    'page': ['create', 'edit']
}  #default permits a new member gets
default_page = 1  # default page to land on when no object/instance is specified


#the following data schema may be subclassed or overidden in an app's config.py
class Page(Schema):
    table = 'pages'
    code = TAG, KEY
    parent = INT, KEY  # defines a hierarchy
    lineage = STR
    name = TAG, KEY
    kind = TAG, KEY
    stage = TAG, 'live', KEY
    when = DATE, now
    text = TEXT #, KEY
    seq = INT, KEY
    rating = INT
    score = INT
    prefs = TEXT
    insert = [
        dict(uid=1, parent=1, name='root', kind='root', lineage="."),
        dict(uid=2, parent=1, name='admin', kind='admin', lineage=".1."),
    ]
