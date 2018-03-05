"""
evoke request object
"""
import sys
import time
from copy import copy
from urllib.parse import quote
from munch import Munch, DefaultMunch

try:
    import user_agents
except ImportError:
#    print("""user_agents module not found. See https://pypi.python.org/pypi/user-agents/""")
    user_agents = None

# Session based cache
from zope.interface import Interface, implementer
from twisted.python.components import registerAdapter
from twisted.web.server import Session

from evoke.lib import DATE, httpDate


class ISessionCache(Interface):
    "Session Cache interface"


@implementer(ISessionCache)
class SessionCache(object):
    "arbitary session storage"

    def __init__(self, session):
        ""


# register session cache
# plumbing
registerAdapter(SessionCache, Session, ISessionCache)

#print "registerAdapter"


class Req(DefaultMunch):
    """ extend the dict object with our own extra methods
    """
    __default__ = ''

    def redirect(self, url, permanent=False, anchor=""):
        """Cause a redirection without raising an error 
    - accepts full URL (eg http://whatever...) or URI (eg /versere/Account/2/view)
    - retains anchor and messages
    """
        if type(url) == bytes:
            url = str(url, 'utf8')
        url = str(url).strip(
        )  #safety first - defend against unicode, which will break both http and string matching
        #    print "REDIRECT:",url
        if not url.startswith('http'):
            url = "http://%s%s" % (self.get_host(), url)

#    print "REDIRECT full url:",url or "no url", " == anchor: ", anchor or "no anchor"
        if '#' in url:
            url, anchor = url.rsplit('#', 1)
        ats = self.error and ['error=%s' % quote(self.error)] or []
        if self.warning:
            ats.append('warning=%s' % quote(self.warning))
        if self.message:
            ats.append('message=%s' % quote(self.message))
        q = url.find('?') > -1 and "&" or "?"
        ats = ats and (q + '&'.join(ats)) or ""
        url = '%s%s%s%s' % (url, ats, anchor and '#' or '', anchor)
        # do the redirect
        self.request.setResponseCode(permanent and 301 or 302, None)
        if type(url) != bytes:
            url = bytes(bytearray(url, 'utf8'))
        self.request.setHeader('Location', url)
        return " "  #return a True blank string, to indicate we have a page result

    def set_cookie(self,
                   id,
                   data="",
                   expires=None,
                   domain=None,
                   path="/",
                   max_age=None,
                   comment=None,
                   secure=None):
        """set defaults, translate expires from seconds to http date,
       and call the twisted method"""
        when = expires and (httpDate(time.time() + expires, rfc='850')) or None
        self.request.addCookie(id, data, when, domain, path, expires
                               or max_age, comment, secure)

    def clear_cookie(self, id):
        "cookie is cleared by setting it to expire a year ago!"
        self.set_cookie(id, expires=-3600 * 24 * 365, max_age=0)

    def get_host(self):
        "host domain"
        host = self.request.requestHeaders.getRawHeaders('host')[0]
        try:
            host = self.request.requestHeaders.getRawHeaders(
                'x-forwarded-host')[0]
        except:
            pass
        return str(host)

    def get_path(self):
        "path"
        return str(self.request.path, 'utf8')

    def get_uri(self):
        "uri"
        return str(self.request.uri, 'utf8')

    def __copy__(self):
        "make this copyable by copy.py"
        return self.__class__(copy(dict(self)))

    def __deepcopy__(self, memo):
        "make this copyable by copy.py"
        return self.copy()


#class OLDReq(Reqbase):
#    """dict / object hybrid - CJH- stores form fields as a dictionary,
#  but allows them to be set and got as req properties.
#  WEAKNESS: if a form field uses a string method name as a key,
#  then that field can only be set and got using req.[key] syntax
#  ADVANTAGE:  allows backward compatibility, and the direct use of
#  dictionary methods and operands
#  """
#
#    def __getattribute__(self, k):
#        if hasattr(Reqbase, k):
#            return dict.__getattribute__(self, k)
#        else:  # do an implicit get
#            # TODO - should this raise an error for an unknown key?
#            return self.get(k, "")
#
#    def __setattr__(self, k, v):
#        if hasattr(Reqbase, k):
#            dict.__setattr__(self, k, v)
#        else:
#            self[k] = v


def debyte_dict(d, exceptions=('filedata', )):
    """convert keys and values from bytes into utf8 strings"""
    res = {}
    for _k, _v in d.items():

        # convert all keys and non-exceptional values to unicode
        k = str(_k, 'utf8')
        if type(_v) == list:
            if k in exceptions:  # this is raw data and not a string
                if len(_v) > 1:
                    v = _v
                else:
                    v = _v[0]
            elif len(_v) > 1:
                v = [str(i, 'utf8') for i in _v]
            else:
                v = str(_v[0], 'utf8')
        else:
            v = str(_v, 'utf8')
        res[k] = v
    return res


def respond(request, dispatcher):
    "initialise req from the Twisted request, then call the dispatcher"
    # First we need to transform the request into our own format
    # our format is {key:value, .., cookies:{key:value}, request:request}
    req = Req()
    # retain multiple value args as a list
    fields = debyte_dict(request.__dict__['args'])
    req.update(fields)

    req.cookies = (request.__dict__['received_cookies'] or {})

    # if we have the relevant modules then add user agent information
    if user_agents:
        ua_string = request.getHeader('user-agent')
        req.user_agent = user_agents.parse(ua_string or '')
    else:
        req.user_agent = None
    req.request = request

    # set up Session cache
    session = request.getSession()
    #req.cache = ISessionCache(session)
    req.cache = session.cache

    # get the domain and port
    req._v_domain = req.get_host().split(":")[0]  # excludes port
    # Now process the request
    path = request.__dict__['path']
    try:
        result = dispatcher.request(path, req)
    except:
        raise
        sys.stderr.write(DATE().time())
        sys.stderr.write(path + '\n')
        result = "request error..."
    return result


def test():
    ""
    r = Req()
    # non-dict key
    r.pie = 400
    assert r.pie == r['pie'] == 400, "non dict keys noworks"
    # dict key
    r.copy = 401
    assert 'copy' not in r, "dict keys should not be items"
    assert r.copy == 401, "dict keys should be set as attributes"
    assert r.wrong == "", "invalid attribute should return an empty string "


if __name__ == '__main__':
    test()
