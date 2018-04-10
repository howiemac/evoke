""" evoke User object

IHM 2/2/2006 and thereafter
CJH 2012 and therafter

gives session-based user validation

The database users table must have an entry with uid==1 and id==guest. 
This is used to indicate no valid login.

The database users table must have an entry with uid==2 . This is the 
sys admin user.

Registration is verifed via email.

Where a user has a stage of "" (the default), this indicates that they 
have not yet had their registration verified, and they will be unable 
to login.

"""

import time
import re
import inspect
import crypt
import uuid
import hashlib
from os.path import dirname
from base64 import urlsafe_b64encode as encode, urlsafe_b64decode as decode

from evoke import lib
from nevo import NevoDecorator
import evoke
html = NevoDecorator.make('User', ['.', dirname(evoke.__file__)])

class User:
    def permitted(self, user):
        "permitted if own record  or got edit permit"
        return self.uid == user.uid or user.can('edit user')

    @classmethod
    def hashed(self, pw, salt=None):
        "return a hashed password prepended with a salt, generated if not specified"
        salt = salt or uuid.uuid4().hex
        return hashlib.sha512(salt.encode() +
                              pw.encode()).hexdigest() + ':' + salt

    def check_password(self, pw):
        "fetch pw from database, split into salt and hash then compare against the pw supplied"
        hashed = self.pw or self.hashed("")
        #salt, hash = hashed[:19], hashed[19:]
        hash, salt = hashed.split(':')
        res = self.hashed(pw, salt) == hashed
        return res

    @classmethod
    def fetch_user(cls, id):
        "return User object for given id, or return None if not found"
        users = cls.list(id=id)
        return users and users[0] or None

    @classmethod
    def fetch_user_by_email(cls, email):
        "return User object for given email, or return None if not found"
        users = cls.list(email=email)
        return users and users[0] or None

    @classmethod
    def fetch_if_valid(cls, id, pw):
        "authenticate password and id - return validated user instance"
        if id:
            user = cls.fetch_user(id)
            #      print "VERIFIED",user.id,user.pw,id,pw, " mode:",getattr(user,'mode','NO MODE')
            if user and user.check_password(pw) and (user.stage == 'verified'):
                return user  #valid
        return None  #invalid

    @classmethod
    def create(cls, req):
        "create a new user, using data from req"
        self = cls.new()
        self.store(req)  #update and flush
        return self

    def store(self, req):
        "update a user, using data from req"
        self.update(req)
        self.flush()
        return self

    def remove(self, req):
        "delete an unverified user - called from Page_registrations.evo"
        if self.stage != 'verified':
            self.delete()
            req.message = '"%s" has been deleted' % self.id
        return self.view(req)

    remove.permit = 'edit user'

    def send_email(self, subject, body):
        ""
        print("email: ", self.Config.mailfrom, self.email)
        lib.email(self.Config.mailfrom, self.email, subject, body)

###### permits ########################

    def is_guest(self):
        ""
        return self.uid == 1

    as_guest = is_guest  # this can be overridden elsewhere, to allow an "as_guest" mode, for non-guest users

    def is_admin(self):
        "system admin?"
        return self.uid == 2

    def can(self, what):
        """
    permit checker - replacement for ob.allowed() which is no more (RIP...)
    - `what` can be a permit, in the form "task group" 
    - `what` can be a method, in which case the permit of that method is checked, and the permitted() method of its class.
    - old form method permits (ie "group.task") are also supported
    - a user can have a master group, which gives unlimited access
    DO NOT CALL THIS METHOD FROM WITHIN A CLASS permitted METHOD or RECURSION WILL BE INFINITE!
    """
        if "master" in self.get_permits():
            return 1
        if inspect.ismethod(what):
            permit = getattr(what.__func__, 'permit', None)
            if permit == 'guest':
                return 1  # ok regardless, if explicit guest permit
            if type(what).__name__ == 'instancemethod':
                if not (inspect.isclass(what.__self__)
                        or what.__self__.permitted(self)):
                    #          print ">>>>>>>>>>>>> method",what,'NOT PERMITTED'
                    return 0
            if not permit:
                return 1  #ok if permitted and no permit set
        else:
            permit = what
        if permit.find('.') > -1:  #retro compatibility
            group, task = permit.split(".", 1)
        else:
            task, group = permit.split(" ", 1)
#    print ">>>>>>>>>>>>> string",what,task,group,task in self.get_permits().get(group,[]),self.get_permits().get(group,[])
        return task in self.get_permits().get(group, [])

    def get_permits(self):
        "returns the permits for a user, as a dictionary of {group:[tasks]}"
        if not hasattr(self, "permits"):
            self.permits = {}
            for k, v in (
                (i['group'], i['task'])
                    for i in self.Permit.list(asObjects=False, user=self.uid)):
                if k in self.permits:
                    self.permits[k].append(v)
                else:
                    self.permits[k] = [v]
        return self.permits

    def store_permits(self):
        "stores the permit dictionary (group:[tasks]}"
        # clear out existing permits for this user (only those in Config.permits, as other permits may be there also, and these should be retained)
        for group, tasks in list(self.Config.permits.items()):
            self.list(
                asObjects=False,
                sql=
                'delete from %s where user="%s" and `group`="%s" and task in %s'
                % (self.Permit.table, self.uid, group, lib.sql_list(tasks)))
        # store the new permits
        for group, tasks in list(self.permits.items()):
            for task in tasks:  # store the permit
                permit = self.Permit.new()
                permit.user = self.uid
                permit.group = group
                permit.task = task
                permit.flush()

    def sorted_permit_items(self):
        "sorts Config.permits.items() so that master comes first"
        return sorted(
            list(self.Config.permits.items()),
            (lambda x, y: (x[0] == 'master' or x < y) and -1 or 0))

    def create_permits(self):
        "creates permits"
        self.stage = 'verified'
        self.flush()
        self.permits = self.Config.default_permits  #set opening permits
        self.store_permits()

    ###################### user validation ######################

    def hook(self, req, ob, method, url):
        """req hook - to allow apps to add attributes to req 
    This is called by dispatch.py, for req.user, immediately after calling req.user.refresh() - so
     req.user can alse be modifed reliably via this hook. 
    """
        pass

    refresh = hook  # backwards compatibility (IHM 2014), in case refresh has been overridden by an app

    @classmethod
    def validate_user(cls, req):
        "hook method to allow <app>.User subclass to override the default validation and permit setting"
        # TODO REMOVE THIS BAD SPIKE
        req.user = cls.validated_user(req)
        req.user.get_permits()
#    print "req.user set to: ",req.user

    @classmethod
    def validated_user(cls, req):
        """login validation is now handled by Twisted.cred.  If we have got this far
       then the password has been successfully checked and the users id is
       available as req.request.avatarId
    """
        user = cls.Session.fetch_user(req)
        # print "VALIDATED USER:",user.id
        # play around with cookies
        if user.uid > 1 and req.get("evokeLogin"):
            #found a valid user in the request, so set the cookies
            forever = 10 * 365 * 24 * 3600  # 10 years on
            #      req.set_cookie('evokeID',user.cookie_data(),expires=req.get("keepLogin") and forever or None)
            if req.get('evokePersist'):  #user wants name remembered
                #        print "REMEMBER ME"
                req.set_cookie('evokePersist', user.id, expires=forever)
            elif req.cookies.get(
                    'evokePersist'
            ) == user.id:  #user no longer wants name remembered
                req.clear_cookie('evokePersist')
        return user

    def login_failure(self, req):
        "checks login form entries for validity - this is called only for guest user, sometime after validate_user().."
        if '__user__' in req:  #we must have logged in and failed login validation to get here
            user = self.fetch_user(req.__user__)
            if user and not user.stage:
                req.error = 'registration for "%s" has not yet been verified' % req.__user__
            else:  # CJH: not good practice to distinguish which of  username and password is valid, so....
                req.error = "username or password is invalid - please try again - have you registered?"
            return 1
        return 0  #we have a guest and not a login failure

    ######################## form handlers #######################

    def login(self, req):
        ""
        return self.login_form(req)

    login.permit = "guest"

    def logout(self, req):
        "expire the user and password cookie"
        req.clear_cookie('evokeID')
        req.request.getSession().expire()
        if req.return_to:
            return req.redirect(req.return_to)
        req.message = '%s has been logged out' % req.user.id
        return req.redirect(self.fetch_user('guest').url(
            'login'))  #use redirect to force clean new login

    def register(self, req):
        "create new user record"
        if self.Config.registration_method == 'admin':  # registration by admin only
            if not req.user.can('edit user'):
                return self.error(
                    req, 'access denied - registration must be done by admin')
        if 'pass2' in req:  #form must have been submitted, so process it
            uob = self.fetch_user(req.username)
            eob = self.fetch_user_by_email(req.email)
            retry = (req.redo == req.username) and uob and (not uob.stage)
            if not req.username:
                req.error = 'please enter a username'
            elif uob and not retry:
                req.error = 'username "%s" is taken, please try another' % req.username
            elif not re.match('.*@.*', req.email):
                req.error = 'please enter a valid email address'
            elif eob and ((not retry) or (eob.uid != uob.uid)):
                req.error = 'you already have a login for this email address'
            elif not req.pass1:
                req.error = 'please enter a password'
            elif req.pass2 != req.pass1:
                req.error = 'passwords do not match - please re-enter'
            else:  #must be fine
                uob = uob or self.new()
                uob.id = req.username
                uob.pw = self.hashed(req.pass1)  # hash the password
                uob.email = req.email
                uob.when = lib.DATE()
                uob.flush()  #store the new user
                key = uob.verification_key()
                site = self.get_sitename(req)
                if self.Config.registration_method == 'admin':
                    # registration by admin only
                    return uob.verify_manually(req)
                elif self.Config.registration_method == 'approve':
                    # registration with admin approval
                    #  (O/S : this should maybe give email confirmation to the new user when admin verifies them?)
                    admin = self.get(
                        2
                    )  #O/S we should allow a nominated other with  'user edit' permit to act as admin for this purpose....
                    text = """
Hi %s

%s wants to register with us at %s, and gives the following introduction:

----------------------- 

%s

----------------------- 


To approve their registration, simply click the link below:

----------------------- 

http://%s%s

-----------------------
""" % (admin.id, req.username, site, req.story, req.get_host(),
       (self.class_url('verify?key=%s') % key))
                    lib.email(
                        self.Config.mailfrom,
                        admin.email,
                        subject="%s registration verification" % site,
                        text=text)  #send the email
                    return self.get(1).registration_requested(req)
                ################################################

                #else we  assume that registration_method is 'self' (the default)

                # registration with self confirmation via email
                text = """
Hi %s

Thanks for registering with us at %s. We look forward to seeing you around the site.

To complete your registration, you need to confirm that you got this email. To do so, simply click the link below:

----------------------- 

http://%s%s

-----------------------

If clicking the link doesn't work, just copy and paste the entire address into your browser.  If you're still having problems, simply forward this email to %s and we'll do our best to help you.

Welcome to %s.
""" % (req.username, site, req.get_host(),
       (self.class_url('verify?key=%s') % key), self.Config.mailto, site)
                print("!!!!!!!! REGISTRATION !!!!!!!!:%s:%s" % (req.username,
                                                                key))
                lib.email(
                    self.Config.mailfrom,
                    req.email,
                    subject="%s registration verification" % site,
                    text=text)  #send the email
                req.message = 'registration of "%s" accepted' % req.username
                return self.get(1).registered_form(req)
        return self.register_form(req)

    register.permit = "guest"  #dodge the login validation

    def verify(cls, req):
        "called from registration email to complete the registration process"
        try:
            #check key
            # prepare key - need to strip whitespace and make sure the length
            # is a multiple of 4
            key = req.key.strip()
            if len(key) % 4:
                key = key + ('=' * (4 - len(key) % 4))
            req.key = key
            try:
                uid, id, pw = decode(req.key).split(',')
            except:
                uid, id, pw = decode(req.key + '=').split(
                    ','
                )  # bodge it... some browsers dont return a trailing '='
#      print '>>>>>',uid,id,pw
            self = cls.get(int(uid))
            if (self.id == id) and (self.pw == pw):
                if not self.stage:  # not already verified, so ..
                    req.__user__ = id
                    req.__pass__ = pw
                    self.create_permits()
                if self.Config.registration_method == 'self':
                    self.validate_user(req)  #create the login cookie
                    return req.redirect(
                        self.url(
                            "view?message=%s" %
                            lib.url_safe('your registration has been verified')
                        ))  #use redirect to force clean new login
                else:
                    return req.redirect(
                        self.url("view?message=%s" % lib.url_safe(
                            'registration of "%s" has been verified' % id)))
        except:
            raise
        return self.error('verification failure')

    verify.permit = 'guest'
    verify = classmethod(verify)

    def verify_manually(self, req):
        "manually verify a registration"
        if not self.stage:
            self.create_permits()
            req.message = 'registration for "%s" has been verified' % self.id
        return self.view(req)

    verify_manually.permit = 'edit user'

    def verification_key(self):
        ""
        return encode("%s,%s,%s" % (self.uid, self.id, self.pw))

    # TODO - password reset mechanism
    def reminder(self, req):
        "send password reminder email"
        return ''

        #self.logout(req)
        #    print "User.reminder"
        if 'id' in req or 'email' in req:  #form must have been submitted, so process it
            # User.reminder req has id or email
            if not (req.id or req.email):
                req.error = 'please enter a registered username or email address'
            else:
                user = self.fetch_user(req.id) or self.fetch_user_by_email(
                    req.email)
                #        print "User.reminder user=", user, user.uid, user.email
                if not user:
                    req.error = '%s is not registered' % (
                        req.id and "username" or "email address", )
                else:  #must be fine!
                    user.send_email('%s password reminder' % user.id,
                                    'your password for %s is: %s' %
                                    (req.get_host(), user.pw))
                    req.message = 'your password has been emailed to you'
                    return req.redirect(
                        self.Page.get(1).url('view?message=%s' % lib.url_safe(
                            req.message)))  #  redirect to check permissions
        return self.reminder_form(req)

    reminder.permit = "guest"  #dodge the login validation

    ###### user admin ######################

    def edit(self, req):
        "edit user details, including permits"
        if 'pass2' in req:  #form must have been submitted, so process it
            if self.uid == req.user.uid:  #ie if editing your own permissions
                req['user.edit'] = 1  #for safety - dont allow you to lose your own security access
            if 'pw' in req and not req.pw:  #no password entered, so don't change it
                del req["pw"]
            if self.Config.user_email_required and not re.match(
                    '.*@.*', req.email):
                req.error = 'please enter a valid email address'
            elif self.Config.user_email_required and (
                    self.email !=
                    req.email) and self.fetch_user_by_email(req.email):
                req.error = 'you already have a login for this email address'
            elif req.pass2 != req.pass1:
                req.error = 'passwords do not match - please re-enter'
            else:  #must be fine!
                if (self.uid > 2) and req.user.can(
                        'edit user'
                ):  # if not admin user, and can edit users, then update permits
                    self.permits = {}
                    for group, tasks in list(self.Config.permits.items()):
                        for task in tasks:
                            if req.get(group + '.' + task):
                                if group in self.permits:
                                    self.permits[group].append(task)
                                else:
                                    self.permits[group] = [task]
                    self.store_permits()
                if req.pass1:
                    self.pw = self.hashed(req.pass1)
                self.store(req)
                req.message = 'details updated for "%s"' % self.id
                #following not needed for session-based login
                ##        if self.uid==req.user.uid:
                #          if self.pw!=req.user.pw:#user is altering own details, so fix the login
                #            req.__user__=self.id
                #            req.__pass__=self.pw
                #            self.validate_user(req) #create the login cookie
                return self.finish_edit(req)  #redirects appropriately
        return self.edit_form(req)

    edit.permit = 'edit user'

    def finish_edit(self, req):
        "returns to user menu (if allowed)"
        if req.user.can('edit user'):
            return self.redirect(req, 'registrations')
        return self.redirect(req)

    ########## utilities ########

    def get_HTML_title(self, ob, req):
        "HTML title - used by wrappers - uses req.title if it exists, otherwise ob.get_title() if it exists"
        return "%s %s" % (
            self.get_sitename(req),
            req.title or (hasattr(ob, "get_title") and ob.get_title()) or "", )

    def get_sitename(self, req):
        "used in emails, HTML title etc."
        return self.Config.sitename or req.get_host()

    ########## landing places  ##################

    @classmethod
    def welcome(self, req):
        "the welcome page, when no object/instance is specified in the URL"
        if req.return_to:
            #      print('REDIRECT c', req.return_to, type(req.return_to))
            return req.redirect(req.return_to)
        url = self.Page.get(self.Config.default_page).url()
        #    print('REDIRECT c', url)
        return req.redirect(url)

# or use this if Page is not installed or in use:
#    return self.get(1).view(req)

    def view(self, req):
        ""
        if self.uid == 1:
            return self.registrations(req)
        return self.edit_form(req)

    home = view

    ################# errors and messages ################

    @classmethod
    def error(self, req, errormsg=''):
        ""
        req.error = errormsg or req.error or 'undefined error'
        try:
            return req.user.error_form(req)
        except:
            return req.error

    @classmethod
    def ok(self, req, msg=''):
        ""
        req.message = msg or req.message or ''
        return req.user.error_form(req)

    ######################## forms #######################

    @html
    def error_form(self, req):
        pass

    @html
    def login_form(self, req):
        req.title = 'login'

    @html
    def register_form(self, req):
        pass

    @html
    def registered_form(self, req):
        pass

    @html
    def registration_requested(self, req):
        pass

    @html
    def registrations(self, req):
        "listing of user registrations, allowing verification"
        req.items = self.list(orderby='uid desc')

    registrations.permit = 'edit user'

    @html
    def reminder_form(self, req):
        pass

    @html
    def edit_form(self, req):
        pass
