from twisted.web.server import Session


class Browsersession(Session):
    """"""
    @classmethod
    def __init__(self, site, id, reactor=None):
        """initialise EvokeSession object"""
        if reactor is None:
            from twisted.internet import reactor
        self._reactor = reactor

        ob = self.new()  # triggers get
        ob.site = site
        ob.id = id
        ob.shadow_id = id
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
