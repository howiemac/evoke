""" Provide URLs for the current context
"""
from urllib.parse import urlencode, quote


class Url:
    ""
    __implements__ = 'UrlInterface'

    def url(self, method="", classname="", **atts):
        "return url for this method"
        cls = classname or self.__class__.__name__  #classname allows override with a subclass or metaclass
        prefix = self.Config.urlpath  # the optional leading prefix e.g. "/evoke"
        # if we can't find a uid, return a classurl instead
        try:
            # if the uname field exists and is populated use the name
            # otherwise use the uid
            if 'uname' in self._v_fields and self.uname.strip():
                uid = self.uname
            else:
                uid = self.uid
            if cls == self.Config.default_class:
                u = "%s/%s%s%s" % (prefix, uid, method and "/" or "", method)
            else:
                u = "%s/%s/%s%s%s" % (prefix, cls.lower(), uid,
                                      method and "/" or "", method)
            if atts:
                u += '?' + urlencode(atts)
            return u
        except:
            return self.classurl(method)
    
    _url = url  # because url is an obvious field name

    @classmethod
    def class_url(self, method, **atts):
        "return url for classmethod"
        cls = self.__name__
        prefix = self.Config.urlpath  # the optional leading prefix e.g. "/evoke"
        if cls == self.Config.default_class:
            u = "%s/%s" % (prefix, method) if prefix else method
        else:
            u = "%s/%s/%s" % (prefix, cls.lower(), method)
        if atts:
            u += '?' + urlencode(atts)
        return u

    classurl = class_url  #backward compat

    def full_url(self, method=""):
        "return full url for method, for external use"
        return self.Config.urlhost + self.url(method)

    @classmethod
    def abs_url(self, url=""):
        "return local absolute url for one that was relative to app path"
        if url.startswith('http') or url.startswith('/'):
            return url
        return self.Config.urlpath + '/' + url

    def external_url(self, url=""):
        "return full url, for external use"
        if not url.startswith(
                '/'
        ):  # we have either a URL relative to this object, or else we already have a full url
            return url
        return self.Config.urlhost + url

    def redirect(self, req, method='view', anchor=''):
        """perform a redirect, retaining anchor and messages etc
    DEPRECATED - use req.redirect()
    """
        return req.redirect(self.url(method), anchor=anchor)


if __name__ == '__main__':
    pass
