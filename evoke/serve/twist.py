"""
Twisted interface

"""

import os
from uuid import uuid4
from twisted.application import internet
from twisted.web import server
from twisted.web.resource import Resource
from twisted.internet import defer
from twisted.web.server import Session
from twisted.web.server import NOT_DONE_YET
from twisted.python.log import ILogObserver, FileLogObserver
from twisted.python.logfile import DailyLogFile

import logging
logger = logging.getLogger('session')
handler = logging.FileHandler('../logs/session.log')
formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
handler.setFormatter(formatter)
logger.addHandler(handler)
logger.setLevel(logging.DEBUG)

# try to import resources for gzipping (not available in older twisted versions
try:
    from twisted.web.resource import EncodingResourceWrapper
    from twisted.web.server import GzipEncoderFactory
    has_gzip = True
except ImportError:
    print("no gzip encoding available")
    has_gzip = False

from evoke.serve import respond, Dispatcher

# Twisted interface

class EvokeResource(Resource):
    isLeaf = True

    def render_GET(self, request):
        res = respond(request, self.evokeDispatcher)
        # handle deferred results
        if isinstance(res, defer.Deferred):
            res.addCallback(self.render_deferred, request)
            return NOT_DONE_YET
        else:
            return res

    # TODO: distinguish POST and GET
    render_POST = render_GET
    render_PATCH = render_GET
    render_DELETE = render_GET

    def render_deferred(self, result, request):
        "handle the final result of a deferred chain"
        request.write(result)
        request.finish()


# override Session to give us a longer timeout
class LongSession(Session):
    sessionTimeout = 60 * 60  # in seconds


class EvokeRequest(server.Request):
    """Request modified to use cookie id rather than uid"""

    def getSession(self, sessionInterface=None):
        # Session management
        if not getattr(self, '_v_session', None):
            cookiename = b"_".join([b'EVOKE_TWISTED_SESSION'] + self.sitepath)
            sessionCookie = self.getCookie(cookiename)
            if sessionCookie:
                try:
                    self._v_session = self.site.getSession(sessionCookie)
                except KeyError:
                    pass
            # if it still hasn't been set, fix it up.
            if not getattr(self, '_v_session', None):
                self._v_session = self.site.makeSession()
                self.addCookie(cookiename, self._v_session.id, path=b'/')
        self._v_session.touch()
        if sessionInterface:
            return self._v_session.getComponent(sessionInterface)
        return self._v_session


class EvokeSite(server.Site):
    """A site which uses a persistent Session"""

    def __init__(self, resource, Session, requestFactory=None, *args,
                 **kwargs):
        """"""
        server.Site.__init__(self, resource, requestFactory, *args, **kwargs)
        # THIS NEEDS TO BE THE EVOKE Session not the Twisted one
        self.sessionFactory = Session.Factory
        Session._site = self
        self.sessions = Session.get_sessions()

    def makeSession(self):
        """
        Generate a new Session instance, and store it for future reference.
        """
        uid = self._mkuid()
        session = self.sessions[uid] = self.sessionFactory(self, uid)
        session.startCheckingExpiration()
        return session

application = ""

def start(application, apps=[]):
    "start a twisted instance"
    dispatcher = Dispatcher(apps)  # we only want one instance
    # attach the service to its parent application
    resource = EvokeResource()
    resource.evokeDispatcher = dispatcher
    # serve gzipped content if we can..
    if has_gzip:
        resource = EncodingResourceWrapper(resource, [GzipEncoderFactory()])
    # this assumes that this is a single apps
    # multiserve having been deprecated
    Session = list(dispatcher.apps.values())[0]['Session']
    # set up our server

    fileServer = EvokeSite(resource, Session, requestFactory=EvokeRequest)
    #fileServer = server.Site(resource)

    # start the service
    port = int(list(dispatcher.apps.values())[0]['Config'].port)
    evokeService = internet.TCPServer(port, fileServer)
    evokeService.setServiceParent(application)

    # logging
    # create log dir if necessary
    try:
        os.mkdir('../logs')
    except OSError:
        pass
    logfile = DailyLogFile("twistd.log", "../logs")
    application.setComponent(ILogObserver, FileLogObserver(logfile).emit)
