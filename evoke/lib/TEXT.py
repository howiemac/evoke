"""
This module implements the TEXT class (a.k.a. TEXT), used for transient storage and manipulation of strings in the special Evoke text format.

O/S Currently assumes that Page.py is in use....

implements self.formatted() and self.summarised()

also implements the alternative self.markdown()
"""

import re

from markdown import Markdown
#from html2text import html2text

from .evolinks import EvoLinkExtension
from .STR import STR
from .INT import INT

def markdown(text, req, *a, **k):
    ""
    extensions = k.setdefault('extensions', [])
    extensions.append(EvoLinkExtension())
    md = Markdown(*a, **k)
    md.req = req
    return md.convert(text)

class TEXT(STR):
    """
  Evoke text format handling
  """

    # extra rules for markdown
    image_rule = re.compile(r'(\[IMG )(.*?)(\])')
    table_rule = re.compile(r'(\[TABLE )(.*?)(\])')

    def summarised(self, req, chars=250, lines=3, formatted=True):
        " return summary of formatted text "
        if formatted:
            return self.formatted(req, chars, lines)
        # not formatted - ignore "lines"
        return self[:chars]

    def formatted(self, req, chars=0, lines=0):
        "format in HTML via Markdown"
        self.has_more = False
        # get the text to process
        text = self
        if chars:
            self.has_more = chars < len(self)
            text = self[:chars]
        if lines:
            z = text.splitlines()
            textlines = z[:lines]
            text = "\n".join(textlines) + '\n'
            self.has_more = self.has_more or (lines < len(z))

        def subimage(match):
            "render a [IMG (uid|url) attributes?]"
            source = match.groups()[1]
            # print (match.groups())
            if ' ' in source:
                url, atts = source.split(' ', 1)
            else:
                url, atts = source, ''

            # check for a valid uid
            if lib.safeint(url):
                try:
                    img = req.user.Page.get(lib.safeint(url))
                    url = img.image_or_thumb_url()
                except:
                    raise
                    pass

            return '<img src="%s" %s />' % (url, atts)

        def subtable(match):
            "substitute a table object"
            source = match.groups()[1]
            # check for a valid uid
            if lib.safeint(source):
                table = req.user.Page.override_get(lib.safeint(source))
                return "<div class='display-table'>%s</div>" % (
                    table.text.replace('\n', ' '))
            else:
                return 'TABLE ERROR %s' % str(source)

        text = self.image_rule.sub(subimage, text)
        text = self.table_rule.sub(subtable, text)

        res = markdown(text, req)
        return "<span class='evo-text-%d'>%s</span>" % (self._v_instance.uid, res)
