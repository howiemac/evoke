""" request dispatcher
"""
from mimetypes import types_map
import traceback
import time
import sys
import _thread

from .app import get_apps
from .url import Url
from evoke.lib import DATE, httpDate, send_error  # , Error
from evoke.data import RecordNotFoundError
from .parse import Parser


class dispatcherCode(Parser):
    ""

    def request(self, uri, req):
        """wrap the result as bytes"""
        res = self._request(uri, req)
        if type(res) == bytes:
            return res
        else:
            return bytes(bytearray(str(res), 'utf8'))

    def _request(self, uri, req):
        ""
        # don't start anything during an update
        if self.locked():
            self.acquire()
            self.release()  # and get on with it..

        # confirm domain is valid
        if req._v_domain not in self.apps:
            req.error = 'invalid domain "%s"' % req._v_domain
            return self.doUnknown(req)

        # find and validate the user, and store it as req.user
        self.apps[req._v_domain]["User"].validate_user(req)

        # we will need config below...
        config = self.apps[req._v_domain]['Config']
 
        # handle the flat files first
        # note that apache may have already fielded these
        if uri.startswith(b"/site/"):
            path = '%s%s' % (config.site_filepath, str(uri[6:],'utf8'))
            # print("URI,path >>>> ",str(uri,'utf8'),path) 
            return self.doFlatfile(req, path)
        elif uri == b"/favicon.ico" or \
           uri.endswith(b'.html'):
#            path = '%s%s' % (config.site_filepath[:-1], str(uri, 'utf8'))
            path = '%s%s' % (config.site_filepath, str(uri[1:],'utf8'))
            # print("URI,path >>>> ",str(uri,'utf8'),path) 
            return self.doFlatfile(req, path)

        # remove the prefix (urlpath), if any
        if config.urlpath and uri.startswith(
                bytes(bytearray(config.urlpath, 'utf8'))):
            URI = uri[len(config.urlpath):]
        else:
            URI = uri

        # parse the request
        # run the URI through the parser
        uri_type, cls, uid, method = self.parseUri(URI)
        # save the method, in case we need it later (eg for return_to)
        req._v_method = method
        # get the default class, if any
        cls = cls or config.default_class

        # and dispatch appropriately...
        if uri_type == 'obj':
            return self.doObject(req, cls, uid or 1, method or 'view', uri)
        elif uri_type == 'namedobj':
            return self.doNamedObject(req, cls, uid or 1, method or 'view',
                                      uri)
        elif uri_type == 'cls':
            return self.doClass(req, cls, method, uri)
        elif uri_type == 'export':
            return self.doExport(req, cls, uid, method, uri)
        elif uri_type == 'default':
            return self.doDefault(req)
        else:
            req.error = "invalid URI type %s" % uri_type
            return self.doUnknown(req)


    def doObject(self, req, cls, uid, method, url):
        ""
        try:
            # be lax about class case sensitivity in urls...
            ob = self.apps[req._v_domain][cls.capitalize()].get(int(uid))
            return self.doMethod(req, ob, method.replace(".", "_"), url)
        except Exception as e:
            msg = "ERROR with %s %s: %s" % (cls, uid, e)
            print(msg)
            raise
            #      req.error="unknown object %s %s" % (cls, uid)
            req.error = msg
            return self.doUnknown(req)

    def doNamedObject(self, req, cls, uid, method, url):
        "Handle object referenced by unique name not uid"
        try:
            klass = self.apps[req._v_domain][cls.capitalize()]
            # make sure this klass has a uname field
            if 'uname' not in klass._v_fields:
                req.error = "%s does not support named objects" % cls
                return self.doUnknown(req)

            ob = klass.list(
                uname=uid)[0]  # this assumes that uname is always unique
            return self.doMethod(req, ob, method.replace(".", "_"), url)
        except:
            req.error = "unknown object %s/%s" % (cls, uid)
            return self.doUnknown(req)

    def doClass(self, req, cls, method, url):
        ""
        try:
            # be lax about class case sensitivity in urls...
            ob = self.apps[req._v_domain][cls.capitalize()]
        except:
            req.error = "unknown class %s" % cls
            return self.doUnknown(req)
        return self.doMethod(req, ob, method.replace(".", "_"), url)

    def doExport(self, req, cls, uid, method, url):
        ""
        # be lax about class case sensitivity in urls...

        ob = self.apps[req._v_domain][cls.capitalize()].get(int(uid))
        return self.doMethod(req, ob, 'export_eve', url)

    def guest_allowed(self, req, fn, ob):
        'is guest allowed any further?'
        if self.apps[req._v_domain]['Config'].guests:
            return 1  # guests allowed regardless
        permit = getattr(fn, 'permit',
                         None)  # is function permit set to "guest"?

        # check for fn.condition
        condition = False
        if hasattr(fn, 'condition'):
            condition = fn.condition(ob)

        return (permit == 'guest') or condition

    def doMethod(self, req, ob, method, url=''):
        "call the requested method, if permitted"
        # allow for .csv and other methods which have a dot extension
        # so the browser knows what to do
        method = method.replace(".", "_")
        # prevent browser from using cache
        # (note: could use Cache-control must-revalidate if this proves to
        # be not strong enough)
        # expired a year ago!
        req.request.setHeader('expires',
                              httpDate(time.time() - (3600 * 24 * 365)))
        # check that function exists
        fn = getattr(ob, method, None)
        if fn is None:
            req.error = "unknown method %s" % method
            return self.doUnknown(req)
        # check user rights
        if req.user.is_guest() and \
           (req.user.login_failure(req) or not self.guest_allowed(req, fn, ob)):
            #      print ('USER X', req.user.is_guest(), req.user.login_failure(req), self.guest_allowed(req, fn, ob))
            req.return_to = req.get_uri(
            )  # makes login return to the desired page
            return req.user.login(req)
        # check permits for this method, and do it!
        # give a hook for apps to add attributes to req at this point
        req.user.hook(req, ob, method, url)
        if req.user.can(fn):
            #     print req.user.id, repr(req.request)
            try:  # return the result of the function
                return fn(req)

#      except RecordNotFoundError, e:
#        #return req.user.error(req,str(e))
#        return req.user.error(req, "record not found")
            except Exception as e:  # describe an application error message
                print('============= TRACEBACK ================')
                sys.stderr.write(DATE().time() + '\n')
                print("ERRB", url, type(url))
                try:
                    sys.stderr.write(url + '\n')
                except:    
                    sys.stderr.write(url + b'\n')
                traceback.print_exc(file=sys.stderr)
                sys.stderr.write('%s\n' % e)
                print('============= END ================')
                send_error(ob, e, sys.exc_info())
                #       return req.user.error(req,
                #                             """application error
                #                             - please contact the system administrator""")
                raise
                return req.user.error(req, "error: %s" % e)
        else:
            req.error = "you do not have permission to access the requested page"
            req.return_to = req.get_uri(
            )  # makes login return to the desired page
            return req.user.login(req)

    def doFlatfile(self, req, name):
        '''
    return flat file
    BEWARE: assumes that the file won't change for a week
    '''
#        print ("flat file name >>>>>:", name)
        try:
            kind = name.rsplit('.', 1)[1].lower()
            mime = (kind == 'ico') and 'image/x-icon' or types_map.get('.'+kind) \
                or 'text/plain'  # don't know why '.ico' is missing from types-map...

            data = open(name, 'rb').read()
            req.request.setHeader('content-type', mime)
            # prevent browser from asking for image every page request
            # assumes won't change for a week!
            req.request.setHeader('expires',
                                  httpDate(time.time() + (3600 * 24 * 7)))
            return data
        except:
            raise
            req.request.setResponseCode(404, b"file not found")
            # we generally don't want a fancy rendered error page here
            return "file not found"

    def doDefault(self, req):
        "give the default URL"
        return self.doMethod(req, req.user, 'welcome')

    def doUnknown(self, req):
        ""
        msg = req.error or "resource not found"
        req.request.setResponseCode(404, bytes(bytearray(msg, 'utf8')))
        try:
            return req.user.error(req, msg)
        except:  # we must have no valid user
            return msg


class dispatcherInit(object):
    ""

    def __init__(self, apps=[]):
        "system start-up"
        # set up the application and the security checks
        self.apps = get_apps(apps)  # build the app classes
        self._sync = _thread.allocate_lock()
        self.acquire = self._sync.acquire
        self.release = self._sync.release
        self.locked = self._sync.locked
        # license
        config = list(self.apps.values())[0]['Config']
        print(config.copyright)
        # port
        print('serving on port %s' % config.port)


class Dispatcher(Url, dispatcherCode, dispatcherInit):
    "combine component classes"
    pass
