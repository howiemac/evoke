""" Browser session - used for login etc
"""
from datetime import datetime, timedelta
import logging

from twisted.web.server import Session as TwistedSession
from twisted.python import components

logger = logging.getLogger('session')
handler = logging.FileHandler('../logs/session.log')
formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s')
handler.setFormatter(formatter)
logger.addHandler(handler)
logger.setLevel(logging.INFO)

logger.debug('started')


class Session(object):
    ""

    @classmethod
    def fetch_user(self, req):
        "get or create session object by id, validate and return the related user"
        logger.debug('fetch_user')

        guest = self.User.get(1)  # fetch_user('guest')

        # the user will either be identified by a pre-identified req.request.avatarId
        # or the req will include __user__ and __pw__
        # FOR NOW: avatarId takes precedence;
        nocred = False
        avatarId = getattr(req.request, 'avatarId',
                           None)  #req.request.avatarId
        if avatarId and avatarId != 'guest':
            user = self.User.fetch_user(
                avatarId
            ) or guest  # fetch_user returns none if user not recognised
            logger.debug("we have an avatar: ", avatarId, " maps to user ",
                         user.id)
        elif '__user__' in req and '__pass__' in req:
            user = self.User.fetch_if_valid(
                req['__user__'],
                req['__pass__']) or guest  # returns guest if not valid
            logger.debug("we have a login, __user__ ", req['__user__'],
                         " maps to user ", user.id)
        else:
            user = guest
            nocred = True
            logger.debug(
                "no credentials - we assume guest unless the session tells us otherwise"
            )
            logger.debug("-->" + str(req.keys()))

        id = req.request.getSession().id

        # look for an existing session
        sessions = self.list(id=id, stage='')

        if sessions:
            logger.debug('found a session, %s' % id)
            session = sessions[0]

            # if the session is new and empty, populate it.
            if not session.ip:
                session.user = user.uid
                session.ip = req.request.getClientIP()
                session.forwarded = self.get_forwarded(req)
                session.expires = datetime.now() + timedelta(days=1)
                session.flush()

            # check the request against the content of the session
            criteria = [
                session.ip == req.request.getClientIP(),
                session.forwarded == self.get_forwarded(req)
            ]
            # if any of our checks fail expire the session
            # exclude brand new session records without ip
            if not all(criteria):
                logger.debug('session expired %s %s %s' %
                             (id, session.ip, self.get_forwarded(req)))
                req.request.getSession().expire()
                logger.debug("ip address didn't match.")
                return guest

            # user identity:  if the session user is guest but another user has
            # been identified in the request we detect a login and update the
            # session's user.  If the user doesn't match we expire the
            # session and return guest (uid=1)

            # session.user is guest and req.user is guest:  return guest

            if session.user == guest.uid and user.uid == guest.uid:

                logger.debug("definitely a guest")
                return guest

            # session has a user and no credentials have been offered
            # return the user specified in the session
            if session.user != guest.uid and nocred:
                logger.debug(
                    "no credentials provided but the session has a valid user %d"
                    % session.user)

                # mirror the session id
                req.set_cookie('mirror', session.id, max_age=60, path='/')

                return self.User.get(session.user)

            # session user != guest and req.user differs: expire session, return guest
            if session.user != guest.uid and session.user != user.uid:
                logger.debug("user doesn't match the session")
                req.request.getSession().expire()
                return guest

            # session.user is guest and req.user != guest: update.session user and return user
            if session.user == guest.uid and user.uid != guest.uid:
                logger.debug("user matches the session")
                session.user = user.uid
                session.flush()
                return user

        else:
            # Since sessions are created earlier when the session
            # cookie has been set this section should never
            # be reached.
            logger.debug("creating new session", id)
            session = self.new()
            session.id = id
            session.user = user.uid
            session.ip = req.request.getClientIP()
            session.forwarded = self.get_forwarded(req)
            session.expires = datetime.now() + timedelta(days=1)
            session.flush()

        return user

    @classmethod
    def get_forwarded(self, req):
        "return the X-Forwarded-For header from the request as a string"
        return str(
            dict(req.request.requestHeaders.getAllRawHeaders()).get(
                'X-Forwarded-For', ''))

    # Twisted view of session
    sessionTimeout = 60 * 60

    @classmethod
    def Factory(self, site, id, reactor=None):
        """initialise EvokeSession object"""
        if reactor is None:
            from twisted.internet import reactor
        self._reactor = reactor

        ob = self.new()  # triggers get
        ob.site = site
        ob.id = id
        ob.flush()
        ob._reactor = reactor
        ob.cache = {}
        self.expireCallbacks = []
        return ob

    @classmethod
    def get_sessions(self):
        """return list of valid sessions"""
        # clear old sessions
        env = dict(session=self.ns_table())
        sql = 'delete from %(session)s where stage="expired"' % env
        self.list(sql=sql)
        session_data = self.list(stage='', asObjects=False)
        sessions = [self.get_session(i['uid']) for i in session_data]
        return dict((str(i.id), i) for i in sessions)

    @classmethod
    def get_session(self, uid, data={}, reactor=None):
        """"""
        ob = self.get(uid, data)
        #components.Componentized.__init__(ob)

        if reactor is None:
            from twisted.internet import reactor
        ob._reactor = reactor

        # TODO - would we ever want callbacks and namespaces to
        # persist across restart.  To my understanding
        # we don't use either yet.
        ob._expireCall = None
        ob.expireCallbacks = []
        ob.touch()
        ob.sessionNamespaces = {}
        ob.cache = {}
        return ob

    def expire(self):
        """"""
        del self._site.sessions[self.id]
        for c in self.expireCallbacks:
            c()
        self.expireCallbacks = []
        if self._expireCall and self._expireCall.active():
            self._expireCall.cancel()
            # Break reference cycle.
            self._expireCall = None
        #self.delete()
        self.stage = 'expired'
        self.flush()

    def touch(self):
        """
      Notify session modification.
      """
        self.lastModified = self._reactor.seconds()
        if self._expireCall is not None:
            self._expireCall.reset(self.sessionTimeout)

        self.flush()

    def startCheckingExpiration(self):
        """
      Start expiration tracking.

      @return: C{None}
      """
        self._expireCall = self._reactor.callLater(self.sessionTimeout,
                                                   self.expire)

    def notifyOnExpire(self, callback):
        """
      Call this callback when the session expires or logs out.
      """
        self.expireCallbacks.append(callback)
