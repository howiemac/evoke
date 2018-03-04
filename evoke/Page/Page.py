# -*- coding: utf-8 -*-
""" EVOKE Page class - allowing several "kinds" of pages, including default kinds:
 - page         : a generic page, which can have child pages (of any kind)
 - file         : a reference to a flat file, available for download its parent page
 - image        : a reference to a an image flat file, available for display on its parent page

Files and images are included as pages so that they can share the same uid sequence and hierarchy.

written by Ian Howie Mackenzie 2006 onwards
"""

# import os
from copy import copy
from os.path import lexists, dirname
from datetime import timedelta, datetime
import pickle as pickle
from io import StringIO

# try to import PyRSS2Gen gracefully
try:
    import PyRSS2Gen
    has_rss = True
except ImportError:
#    print("no rss generator.")
    has_rss = False

# local imports
from .File import File
from .Image import Image
from evoke.lib import *
from nevo import NevoDecorator

import evoke

html = NevoDecorator.make('Page', ['.', dirname(evoke.__file__), './User', ])


class Page(Image, File):
    ""

    #stage constants
    pagestages = ['posted', 'draft']

    #kind constants
    postkinds = ['page']
    contextkinds = [
        'image', 'file'
    ]  #these are typically viewed in the context of their parents
    imageaddkinds = ['page', 'root', 'admin']  # kinds which can have images
    fileaddkinds = imageaddkinds  #kinds which can have child files
    validchildkinds = {'root': ['page'], 'admin': ['page'], 'page': ['page']}


    # #### Kinds - convenience methods to ease listing

    class Kindlist(object):
        """
    Allow us to call Page.kinds.<kind>(**params)
    instead of Page.list(kind=<kind>, **params)
    """

        def __init__(self, Page):
            self.Page = Page

        def __getattr__(self, kind):
            "return a partially applied list function"

            def list(**params):
                # assume we want stage='posted', unless told otherwise
                params.setdefault('stage', 'posted')
                return self.Page.list(kind=kind, **params)

            return list

    @classmethod
    def __class_init__(self):
        """Import additional kind-classes"""
        self.kinds = self.Kindlist(self)


    # overrides

    @classmethod
    def get(self, uid, data={}):
        "override get"
        ob = self.__get__(uid, data)
        # look for override class for this kind
        override_classname = 'Page_%s' % ob.kind.replace(' ', '_')
        if hasattr(ob, override_classname):
            override_class = getattr(self, override_classname)
            bases = tuple([override_class, self] +
                          list(ob.__class__.__bases__))
            # TODO avoid duplicate base names with more elegance
            try:
                ob.__class__ = type(self.__name__, bases, {})
            except TypeError:
                pass
            ob.__override_classname__ = override_classname
        ob.get = self.__get__
        return ob


    def __repr__(self):
        ""
        return """<UID: %d | Name: %s | Kind: %s | Stage: %s>\n""" % (
            self.uid, self.name, self.kind, self.stage)



########## access restrictions ############################

# admin (user.uid==2) has unlimited access
# guests can view only posted material, and cannot edit or add

    def permitted(self, user):
        """ does user have access rights to this page? 
    """
        return (self.stage in [
            "posted", "live"
        ]) or (self.kind == "image") or (user.is_admin())

    def edit_permitted(self, user):
        """ does user have edit rights to this page?
    """
        return (user.is_admin())

    def editable(self, req):
        "is user allowed to edit this page?"
        return self.edit_permitted(req.user)

    @classmethod
    def visible(cls, user, pages):
        "filters out pages user is not allowed access to "
        return [p for p in pages if p.permitted(user)]

    def content_permitted(self, user):
        "can an article be posted here by req.user?"
        return self.edit_permitted(user)

###  utility methods  ########################################################

    def get_pob(self):
        "parent object - cached for efficiency - assumes details wont change during lifespan of instance"
        if not hasattr(self, "pob"):
            self.pob = self.get(self.parent)
        return self.pob

    def get_container(self, immediate=False):
        "returns self, if immediate==True and self is a container, or else the containing ancestor, if there is one"

        # TO BE REWRITTEN......  see get_pref()

        if not hasattr(self, "_container"):
            if immediate and self.kind in self.containerkinds:
                self._container = self
            else:
                self._container = None
                for uid in reversed(self.lineage.split(".")):
                    if uid:
                        c = self.get(int(uid))
                        if c.kind in self.postkinds:
                            self._container = c
                            break
        return self._container

    def get_name(self):
        "gives '(untitled)' where there is no name"
        return self.name or '(untitled)'

    get_title = get_name

    def get_description(self):
        ""
        return "%s %s" % (self.status(full=True) or self.stage, self.kind)

    def status(self, full=False):
        "shows any non-'posted' status"
        s = ""
        if self.stage == "draft":
            s = 'draft'
        return s

    def tagline(self, showtime=True, long=False):
        "name and when: used in templates "
        name = self.Config.attribution == "full" and (
            "by %s" % self.get(2).get_name()) or ""
        #    when=self.get_pref('show_time') and self.when.nice(long=long) or ""
        when = showtime and self.when.nice(long=long) or ""
        return '%s %s' % (name, when)

    def has_children(self):
        ""
        if hasattr(self, "_children"):
            return len(self._children)
        else:
            return self.count(parent=self.uid)

    def get_children(self):
        "gives child objects, cached for efficiency"
        if not hasattr(self, "_children"):
            self._children = self.list(parent=self.uid, orderby="seq,uid")
        return self._children

    @classmethod
    def get_parents(self):
        "returns all parent objects (i.e. parents of further pages-  not of images or files)"
        if not hasattr(self, "_parents"):
            puids = self.list_int(
                'parent', distinct=True, kind='page', orderby="uid")
            if puids:
                self._parents = self.list(isin={'uid': puids})
            else:
                self._parents = []
        return self._parents

    def get_children_by_kind(self, kind="", orderby='seq,uid'):
        "get all children of given (or own) kind"
        return self.list(
            parent=self.uid, kind=kind or self.kind, orderby=orderby)

    def get_minrated_children_by_kind(self, kind="", orderby='seq,uid'):
        "get all children of given (or own) kind"
        return self.list(
            parent=self.uid, kind=kind or self.kind, where=f"rating>={self.minrating()}", orderby=orderby)

    def get_siblings_by_kind(self, kind=""):
        "get list of siblings of given (or own) kind"
        #    return self.list(parent=self.parent,kind=kind or self.kind,where='stage!="dead"',orderby='seq,uid')
        sibs = self.list(
            parent=self.parent, kind=kind or self.kind, orderby='seq,uid')
        return [x.uid == self.uid and self or x
                for x in sibs]  #put self in the list

    def get_older_item(self):
        "for articles and replies - in date then uid order (seq is ignored) - get next visible sibling of same kind as self"
        if self.kind in self.postkinds:
            dt = self.when.sql()
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where='(uid!=%s) and ((`when`<%s) or (`when`=%s and uid<%s))' %
                (self.uid, dt, dt, self.uid),
                limit=1,
                orderby='`when` desc, uid desc')
            return sib and sib[0] or None
        return None

    def get_newer_item(self):
        "for articles and replies - in date then uid order (seq is ignored) - get next visible sibling of same kind as self"
        if self.kind in self.postkinds:
            dt = self.when.sql()
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where='(uid!=%s) and ((`when`>%s) or (`when`=%s and uid>%s))' %
                (self.uid, dt, dt, self.uid),
                limit=1,
                orderby='`when`, uid')
            return sib and sib[0] or None
        return None

    def get_next_alphabetical_item(self):
        "get next sibling, in name order, of same kind as self"
        if self.kind in self.postkinds:
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where='(uid!=%s) and (name>="%s")' % (self.uid, self.name),
                limit=1,
                orderby='name')
            return sib and sib[0] or None
        return None

    def get_previous_alphabetical_item(self):
        "for name order (seq is ignored) - get previous sibling of same kind as self"
        if self.kind in self.postkinds:
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where='(uid!=%s) and (name<="%s")' % (self.uid, self.name),
                limit=1,
                orderby='name desc')
            return sib and sib[0] or None
        return None

    def get_next_uid_item(self):
        "get next sibling, in uid order, of same kind as self"
        if self.kind in self.postkinds:
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where='(uid>%s)' % (self.uid, ),
                limit=1,
                orderby='uid')
            return sib and sib[0] or None
        return None

    def get_previous_uid_item(self):
        "get previous sibling, in uid order, of same kind as self"
        if self.kind in self.postkinds:
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where='(uid<%s)' % (self.uid, ),
                limit=1,
                orderby='uid desc')
            return sib and sib[0] or None
        return None

    def get_next_seq_item(self):
        "get next sibling, in seq order, of same kind as self"
        if self.kind in self.postkinds:
            where = '((seq>%s) or ((seq=%s) and (uid>%s)))' % (self.seq,
                                                               self.seq,
                                                               self.uid)
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where=where,
                limit=1,
                orderby='seq, uid')
            return sib and sib[0] or None
        return None

    def get_previous_seq_item(self):
        "get previous sibling, in seq order, of same kind as self"
        if self.kind in self.postkinds:
            where = '((seq<%s) or ((seq=%s) and (uid<%s)))' % (self.seq,
                                                               self.seq,
                                                               self.uid)
            sib = self.list(
                parent=self.parent,
                kind=self.kind,
                stage='posted',
                where=where,
                limit=1,
                orderby='seq desc, uid desc')
            return sib and sib[0] or None
        return None

    def renumber_siblings(self):
        "numbers the siblings (ie sets the seq)"
        n = 1
        for s in self.get_pob().get_children(
        ):  #relies on them being sorted by seq,uid
            if s.seq != n:
                s.seq = n
                s.flush()
            n += 1

    def renumber_siblings_by_kind(self):
        "numbers the siblings (ie sets the seq)"
        n = 1
        for s in self.get_siblings_by_kind(
        ):  #relies on them being sorted by seq,uid
            #      print ">>>>>>>>>>>>>>>>",s.uid,s.seq,n
            if s.seq != n:
                s.seq = n
                s.flush()
            n += 1

    def get_ancestry(self):
        "convert lineage to list of page objects - returned oldest first - cached"
        if not hasattr(self, "_ancestry"):
            self._ancestry = []
            for uid in self.lineage.split("."):
                if uid:
                    self._ancestry.append(self.get(int(uid)))
        return self._ancestry

    def get_ancestors(self, kind):
        "gets ancestry from (and including) the last occurrence of the given kind - returned oldest first"
        ancs = []
        for uid in reversed(self.lineage.split(".")):
            if uid:
                a = self.get(int(uid))
                ancs.append(a)
                if a.kind == kind:
                    break
        return reversed(ancs)

    def set_lineage(self, pob=None):
        "sets lineage of self - doesn't flush"
        pob = pob or self.get_pob()
        self.lineage = '%s%s.' % (pob.lineage, pob.uid)
#    print ">>>> SET LINEAGE AS",self.lineage

    def set_descendant_lineage(self):
        """calculates, and flushes the lineage for all descendants
    """

        def get_tree(pob):
            children = pob.list(parent=pob.uid)
            for s in children:
                if s.uid > 1:
                    s.set_lineage(pob)
                    #        print ">>>> SET DESCENDANT LINEAGE AS",self.lineage
                    s.flush()
                    get_tree(s)

        get_tree(self)

    def clear_form(self, req):
        "blank the form variables"
        # DO WE NEED THIS ?????
        req.text = req.code = req.kind = req.name = ''

## page creation / maintenance  ###################################

    def set_seq(self):
        "default sequence is generally based on 'when'"
        self.seq = self.when.count()

    def stamp(self):
        "date stamp (ie set 'when'), and set 'seq' - also sets thread latest-reply link (in 'seq') - DOESNT FLUSH SELF"
        self.when = DATE()
        self.set_seq()

# THE FOLLOWING WORKED pre-markdown BUT NEEDS A DIFFERENT TRIGGER (i.e. not **) NOW...
#
#    def expand_text(self, req):
#        "expands ** into child pages"
#        pages = self.text.sectioned()
#        if len(pages) > 1:
#            for s in reversed(pages[1:]):
#                n, t = s.split('\n', 1)
#                self.create_child_page(name=n, text=t)
#        return pages[0]

    def flush_page(self, req):
        ""
        #remove pesky carriage returns!
        self.text = self.text.replace("\r","")
#        self.text = self.expand_text(req)
        self.flush()
#    print "++++++++++++++ per MySQL ++++++++++++++++",self.get(self.uid).text

    def create_page(self, req):
        "generic create for most page kinds (not images or files) "
        req.setdefault('kind', 'page')
        #    self.validate_name(self,req)
        if req.error:
            return None
        # update
        page = self.new()
        page.parent = self.uid  #may be overridden later
        page.update(req)
        page.set_lineage()
        page.stage = req.stage or 'draft'
        page.stamp()
        page.flush_page(req)
        #O/S leave trail entry
        return page

    create_page.permit = "no way"

    def add_page(self, req):
        "generic add for most page kinds (not images or files) "
        page = self.create_page(req)
        if not page:
            return self.view(req)
        # return a redirect to avoid the user refreshing forms or copying invalid links
        return page.redirect(
            req, 'edit')  # default is to return new page in edit mode

    add_page.permit = "create page"

    def save_text(self, req):
        ""
        #    req.text=TEXT(req.text).cleaned() # removed by IHM 12/5/2016 to re-allow use of underlines to extend a style
        self.update(req)
        self.flush_page(req)
        self.clear_form(req)
        if "post" in req:
            return self.post(req)
        if not (req.error or req.message):
            req.message = "text saved at %s" % DATE().time(
                sec=True, date=False)
        return req.redirect(
            self.url('edit' if self.stage == 'draft'
                     else 'view'))  # anchor removed

    ###################### view (and edit) page ##################################

    def get_order(self, pref=''):
        ""
        p = pref or self.get_pref('order_by')
        if p == 'name':
            order = "name"
        elif p == 'latest':
            order = "`when` desc,uid desc"
        elif p == 'seq':
            order = "seq,uid"
        else:  # order_by=='date'
            order = "`when`,uid"
        return order

# old version with "first", and "pagemax" instead of "limit"
#
#  def get_child_pages(self,req,first=False,pagemax=50,descend=False):
#    """pages sequenced according to 'order_by' preference
#    - optional req.year or req.date, or req.match
#    - optional req.limit, or pagemax, or first (mutually exclusive - first always starts at the beginning and gives the number to show)
#    """

    def get_child_pages(self, req, limit=50, descend=False):
        """pages sequenced according to 'order_by' preference 
    - optional req.year or req.date, or req.match
    - optional req.limit or limit (mutually exclusive - akin to pagesize in def page() in lib/library.py)
    """
        if ('limit' in req):
            lim = safeint(req.limit)
        else:  # default
            lim = limit
        order = self.get_order()
        if req.date:  # date in integer yyyymmdd format
            where = "`when`=%d" % safeint(
                req.date)  # date converted to safeint to foil SQL injection!
        elif req.year:
            where = "year(`when`)=%d" % safeint(
                req.year)  # year converted to safeint to foil SQL injection!
        elif req.match:
            where = "name like '%s%%'" % req.match
        else:
            where = ""
        # get the pages
        if descend or self.get_pref(
                'show_descendants'
        ):  # shows every descendant posting you are allowed to see
            items = self._latest(
                req, kinds=self.postkinds, where=where, order=order, limit=lim)
        else:
            #      if first:
            #        lim="0,%s" % pagemax
            #      else:
            #        lim=page(req,pagemax)
            #      items=self.list(parent=self.uid,isin={'kind':self.postkinds},where=where,orderby=order,limit=lim)
            items = self.list(
                parent=self.uid,
                stage='posted',
                isin={'kind': self.postkinds},
                where=where,
                orderby=order,
                limit=page(req, lim))
        if not req.page:
            req.page = 'view'  # for paging
        return items

    def get_branch(self, isin={}, expand=False):
        "recursive fetch of entire branch (includes self) - can be filtered by 'isin' clause - can expand images, files etc to include their file data"

        def get_tree(pob):
            children = pob.list(parent=pob.uid, isin=isin)
            for s in children:
                self.branch.append(s)
                if expand and (s.kind in ['image', 'file']):
                    s.data = s.filedata()
                get_tree(s)

        self.branch = [self]
        get_tree(self)
        return self.branch

    def create_child_page(self, name='', text=''):
        "creates a new child page, and returns it"
        ob = self.new()
        ob.parent = self.uid
        ob.kind = 'page'
        ob.stage = self.stage
        ob.when = DATE()
        ob.name = name
        ob.text = text
        ob.set_lineage()
        ob.flush()
        ob.renumber_siblings_by_kind()
        return ob

    @html
    def view_form(self, req):
        ""
        req.pages = self.get_child_pages(req)  # for Page_view_form.evo
        req.pageuid = self.uid  # for use in templates to test whether a given instance is the page instance
        req.page = "view"
        if not req.return_to:
            req.return_to = self.url(
            )  # allows us to return to this page, if required in template code

    def view(self, req):
        "page view"
        return self.view_form(req)

    @html
    def edit_form(self, req):
        ""
        req.pages = self.get_child_pages(req)  # for template
        req.pageid = 'page_edit'
        req.page = "edit"
        req.edit = self.uid

    def edit(self, req):
        "page edit"
        #make sure we do have permission to edit...
        if not self.editable(req):
            req.error = 'you cannot edit this item'
            return self.view(req)
        # redirect images and files
        if self.kind == 'image':
            self.get_pob().redirect(req, "add_image?edit=%s" % self.uid)
        elif self.kind == 'file':
            self.get_pob().redirect(req, "add_file?edit=%s" % self.uid)
        return self.edit_form(req)

    edit.permit = 'edit page'

    def context(self, req):
        "show in the context of the parent"
        if self.kind in self.contextkinds:  #show in context
            return self.get_pob().redirect(req, 'view', self.uid)
        return self.redirect(req, 'view')

    def swap(self, req):
        "swaps seq of two sibling pages OF THE SAME KIND, allowing rearrangement of list order"
        if req.swap:
            self.renumber_siblings_by_kind()
            swob = self.get(safeint(req.swap))
            z = self.seq
            self.seq = swob.seq
            swob.seq = z
            self.flush()
            swob.flush()
        return req.redirect(self.get_pob().url(
            self.kind in ('file', 'image') and ('add_%s' % self.kind) or
            'edit#me'))

    swap.permit = 'edit page'

#    def toggle_mode(self, req):
#        "mode is stored in the cached user object, for permanence"
#        mode = getattr(req.user, "mode", False)
#        print("BEFORE mode=", mode)
#
#        req.user.toggle_mode()
#        print("AFTER mode=", req.user.mode)
#        url = "%s%s" % (req.return_to, req.user.mode and "/edit" or "")
#        return req.redirect(url)
#
#    toggle_mode.permit = "admin page"

    ############# navbar #################

    def get_navbar_links(self):
        """returns (name,href,title) for each navbar link
       - assumes that navbar items are:
         - the posted child pages of page 1
         - an additions listing link
       - can be overridden by apps, to give whatever is required
    """
        home = self.get(1)
        links = [(p.name, p.url(), p.name)
                 for p in home.list(
                     parent=home.uid,
                     kind='page',
                     stage='posted',
                     where="uid!=2",
                     orderby=home.get_order())]
        links.append(("latest", home.url("latest"), "what's new"))
        return links

    def navbar_links(self):
        """ gets navbar links via cache
    """
        if not hasattr(self, "_navbarlinks"):
            self._navbarlinks = self.get_navbar_links()
        return self._navbarlinks

############# options ################

    def add_option(self, req, label, method="", hint="", url=""):
        """adds pageoption (ie local option) if it is permitted (but if url is used in place of method, it is not checked for permission)
    """
        if ((not method) or req.user.can(getattr(self, method))):
            url = url and self.abs_url(url) or self.url(method)
            act = [label, url, hint or ("%s %s" % (self.get_name(), label))]
            if 'pageoptions' in req:
                req.pageoptions.append(act)
            else:
                req.pageoptions = [act]

    def get_pageoptions(self, req):
        """  page options 
    - this function is called from  Page_header_start.evo to produce page tabs
    """
        # view
        self.add_option(req, self.kind, 'view')
        # edit etc.
        if self.editable(req):
            uid = req.edit or req.view or self.uid
            self.add_option(req, 'edit', 'edit', hint='edit this page')
        # images
        if self.kind in self.imageaddkinds:
            self.add_option(req, "images", "add_image")
        # files
        if self.kind in self.fileaddkinds:
            self.add_option(req, "files", "add_file")
        # prefs
        if self.kind in self.default_prefs:
            self.add_option(req, "preferences", "preferences")
        # owner options
        if self.uid == 2:  # can't use my() function here as different methods are required
            self.add_option(req, 'my details', 'details')
            drafts = self.drafts_count(req)
            if drafts:
                self.add_option(req, 'my drafts (%s)' % drafts, 'drafts')

        # move, copy, export, import moved here for convenience of access - BUT NOTE: SHOULD BE POSTs not GETs (IHM 13/12/2015)
        move = self.get_move(req)
        if move:
            self.add_option(
                req, 'cancel move', 'cancel_move', hint='cancel page move')
            if self.can_move_here(req):
                self.add_option(
                    req,
                    'copy here',
                    'copy',
                    hint='copy page %s here' % move.uid)
                if self.uid not in (move.uid, move.parent):
                    self.add_option(
                        req,
                        'move here',
                        'here',
                        hint='move page %s here' % move.uid)
        else:
            if req.user.can('admin page'):
                self.add_option(
                    req,
                    'move/copy',
                    'move',
                    hint='mark for moving or copying')
# temporarily disable Export/Imprt until it can be fully tested... (IHM Dec 2015)
#     if self.stage!='draft':
#       self.add_option(req,'export','export')
#       self.add_option(req,'import','import_eve')

# remove single tabs
        if len(req.pageoptions) == 1:
            req.pageoptions = []
        # pass back the result
        return req.pageoptions

############### actions ######################

    def add_act(self,
                req,
                label,
                method="",
                confirm="",
                url="",
                hint="",
                hilite=False,
                key=""):
        """adds act if it is permitted (but if url is used in pace of method, it is not checked for permission)
      url will override method, but method can still be given to check permits
    """
        if (not method
            ) or req.user.can(getattr(self, method.split('#', 1)[0])):
            #      url=method and self.url(method) or self.abs_url(url)
            url = url and self.abs_url(url) or self.url(method)
            act = [
                label, url, hint or confirm or method, confirm and
                ("return confirm('are you sure you wish to %s?')" % confirm)
                or "", hilite, key
            ]
            if 'actions' in req:
                req.actions.append(act)
            else:
                req.actions = [act]

    def add_delete(self, req):
        self.add_act(req, 'delete', 'kill', 'delete this %s' % self.kind)

#  def set_listing_actions(self,req):
#    ""

    def get_actions(self, req):
        "actions - note that action button forms should use method='get', as action parameters are passed in the URL"
        # stage changes
        if self.stage == 'posted':
            self.add_act(req, 'withdraw', 'withdraw',
                         'withdraw this %s and all its contents' % self.kind)
        elif self.stage == 'draft':
            if (self.text or self.get_images() or req.pages
                    or req.contents) and not req.edit:
                self.add_act(
                    req,
                    'post',
                    'post',
                    hint='make this %s public' % self.kind,
                    hilite=True)
            self.add_delete(req)

        return req.actions  # TEMPRARY DISABLING OF MOVE/COPY/EXPORT/IMPORT

        # move, copy, export, import
        move = self.get_move(req)
        if move:
            self.add_act(
                req, 'cancel move', 'cancel_move', hint='cancel page move')
            if self.can_move_here(req):
                self.add_act(req, 'copy here', 'copy',
                             'copy page %s here' % move.uid)
                if self.uid not in (move.uid, move.parent):
                    self.add_act(req, 'move here', 'here',
                                 'move page %s here' % move.uid)
        else:
            if req.user.can('admin page'):
                self.add_act(
                    req,
                    'move/copy',
                    'move',
                    hint='mark for moving or copying')
# temrarily disable Export/Imprt until it can be fully tested... (IHM Dec 2015)
#     if self.stage!='draft':
#       self.add_act(req,'export','export')
#       self.add_act(req,'import','import_eve')
# and return
        return req.actions

    def can_move_here(self, req):
        """is it okay to move or copy the move object here?
     - this is a hook for override by inheriting classes"
     - default: can move anything here, provided we have a valid move uid
    """
        return self.get_move(req)

    def _posted(self, req):
        """post a draft (inner workings)
    """
        if self.stage != 'posted':  #safety valve
            self.stage = 'posted'
            self.stamp()
            # store it all
            self.flush()
            req.message = 'your %s is posted' % (self.kind, )
            return True
        return False

    _posted.permit = 'NOWAY'

    def post(self, req):
        """post a draft (requestable)
    """
        if self._posted(req):
            # return the parent page
            return self.context(req)
        #else
        return self.view(req)

    post.permit = 'create page'

    def withdraw(self, req):
        "remove from posted: reset self and all posted descendants back to draft"
        if self.stage == 'posted':
            self.stage = 'draft'
            self.flush()
            #set message
            req.message = 'this %s is now draft' % self.kind
        return self.view(req)

    withdraw.permit = "admin page"

    def kill(self, req):
        "delete self and all childen!"
        if (self.stage == 'draft'):  #safety first
            self.delete_branch()
            message = '%s "%s" has been deleted' % (self.kind, self.name)
        else:
            message = 'deletion denied'
        return req.redirect(
            self.get_pob().url('view?message=%s' % url_safe(message)))

    kill.permit = "create page"  #creator can kill a page, but not if it has been been posted (as she can't withdraw it without admin permit)

    def delete_branch(self):
        "branch deletion - self and ALL child pages of any kind (the whole branch!) are deleted"
        for p in self.get_branch():
            if p.kind == 'image':
                self.get(p.uid).delete_image()
            else:
                p.delete()

    def manage(self, req):
        "link to user edit"
        user = self.User.list(page=self.uid)[0]
        req.page = 'manage'  # tabs need this
        return user.edit(req)

    manage.permit = 'edit user'

    def details(self, req):
        "link to edit of own details"
        req.page = 'details'
        return req.redirect(req.user.url("edit"))

###################### ratings / enable / disable ###################

    ratedkinds=("page","image")
    downratings=(-4,-4,-3,-2,-4,0,1)
    upratings=(0,-2,-1,-1,1,2,2)

# non glyphicon version

    # access these via rating_symbol()
    rating_symbols=('&times;','?','&radic;','&hearts;','?','&radic;','&hearts;')

    def rating_symbol(self,rating=None):
        "give symbol for rating"
        # rating should be in (-4,-3,-2,-1,0,1,2)
        r=min(6,max(0,(rating if rating is not None else self.rating)+4))
        return self.rating_symbols[r]


# glyphicon version

    # access these via rating_class()
    rating_classes=('remove-sign','question-sign','ok-sign','heart','question-sign','ok-sign','heart')

    def rating_class(self,rating=None):
        "give class for rating"
        # rating should be in (-4,-3,-2,-1,0,1,2)
        r=min(6,max(0,(rating if rating is not None else self.rating)+4))
        return "glyphicon glyphicon-%s" % self.rating_classes[r]

# generic

    def set_rating(self,rating):
        "sets self.rating to rating"
        self.rating=rating
        self.flush() 

    def minrating(self):
        "returns (cached) minimum rating accepted by global filter"
        if not hasattr(self, "_v_minrating"):
            self._v_minrating = self.list_int(item='rating',uid=1)[0]
        return self._v_minrating

    def set_global_filter(self,req):
        "sets root rating (used as a global filter) to req.rating"
        self.get(1).set_rating(req.rating)
        return req.redirect(self.url())

    def rate_up(self,req):
        "increase rating"
        try:
            self.rating=self.upratings[self.rating+4]
            self.flush()
        except:
            pass
        return req.redirect(self.url())

    def rate_down(self,req):
        "decrease rating"
        try:
            self.rating=self.downratings[self.rating+4]
            self.flush()
        except:
            pass
        return req.redirect(self.url())

    def toggle_disable(self,req):
        "disable / enable"
        try:
            self.rating=(0,0,1,2,-3,-2,-1)[self.rating+4]
            self.flush()
        except:
            pass
        return req.redirect(self.url())

###################### emails ##########################

    def email_enabled(self):
        ""
        return self.Config.mailfrom and self.Config.SMTPhost and True or False

    def email(self, TO, subject, text='', html=''):
        """convenient wrapper for library email function, supplying the configuration defaults
    Note that if self.Config.mailfrom has a False value, or no SMTPhost is set,  no attempt will be made to send any email
    """
        if self.email_enabled():
            email(
                FROM=self.Config.mailfrom,
                TO=TO,
                subject=subject,
                text=text,
                html=html,
                SMTP=self.Config.SMTPhost,
                LOGIN=self.Config.SMTPlogin)

######################preferences ########################

# O/S : prefs should be stored in a separate table (rather than a column), for more efficient access
# as currently every single pref can require multiple page fetches (up the lineage) to find its value
# Alternatively, in get_pref(), lineage objects containing prefs should be cached when first accessed
#  CONTAINER code elsewhere should be replaced with same LINEAGE approach as in get_pref()

    page_default_prefs = {
        'order_by': ('latest', 'order items by', ('date', 'latest', 'name',
                                                  'seq')),
        'show_time': ('Y', 'show dates and times', 'checkbox'),
        #    'in_menu':('','in menu?','checkbox'),
        'show_descendants': ('', 'show all descendants?', 'checkbox')
    }
    default_prefs = {
        # {kind:{name:(default,display-name,display-type/size/options),},}
        'root': copy(page_default_prefs),
        'admin': copy(page_default_prefs),
        'page': copy(page_default_prefs),
    }

    def get_prefs(self):
        "returns dictionary of page preferences, from cache if possible - will use defaults if no prefs have yet been set"
        #
        # BUG! - THIS SHOULD TRAVERSE THE PREF HIERARCHY WHEN LOCAL PREF IS NOT YET CREATED, i.e. AS PER get_pref()
        #
        # preferences code NEEDS REDESIGN, to recognise  use of empty strings
        # currently, only checkboxes can have an empty string as a valid override preference
        # PREFERENCES SHOULD BE TOTALLY AMALGAMATED WITH Config
        #
        if not hasattr(self, '_prefs'):
            self._prefs = {}
            if self.kind in self.default_prefs:
                defs = self.default_prefs[self.kind]
                if self.prefs:
                    for i in self.prefs.split('\n'):
                        if i:
                            k, v = i.split('=')
                            if k in defs:  # check to skip old preferences that have been removed from defs
                                if not v and (
                                        defs[k][2] != 'checkbox'
                                ):  # non-checkboxes require a value
                                    v = None
                                self._prefs[k] = v
                else:  #prefs not yet created, so use defaults
                    for k, v in list(defs.items()):
                        self._prefs[k] = v[0]
        return self._prefs

    def get_pref(self, pref):
        "returns relevant pref from self.prefs, or container prefs, or Config"
        p = None
        #    print "getting pref: ",pref, " for " ,self.kind,self.uid
        if self.kind in self.default_prefs:  # check own prefs
            p = self.get_prefs().get(pref)
#      print "checking self: ",repr(p)
        if p is None:  # check up along the lineage
            lineage = reversed(self.lineage.strip(".").split("."))
            #     print ">>> lineage = ",list(lineage)
            for l in lineage:
                if l:
                    container = self.get(safeint(l))
                    if container.kind in self.default_prefs:  # check container's prefs
                        p = container.get_prefs().get(pref)
                        #        print "checking lineage: ",container.uid, container.name,"=>", repr(p)
                        if not p is None:
                            break
        if p is None:  # check config
            p = getattr(self.Config, pref, '')
#      print "checking config: ",repr(p)
#    print "GOT ",repr(p)
        return p

    @html
    def preferences(self, req):
        ""
        req.page = 'preferences'

    preferences.permit = 'admin page'

    def update_prefs(self, req):
        "called by Page_preferences.evo: updates self.prefs"
        xprefs = self.get_prefs()
        self.prefs = ''
        for name, defn in list(self.default_prefs[self.kind].items()):
            default, displayname, typ = defn
            value = req.get(name, '').strip()
            #      print "======",name,':',value,' ( ',req.get(name,''),' )'
            self.prefs += '%s=%s\n' % (name, value)
            # make any changes necessary - see change_theme() in music app  as an example
            if (xprefs.get(name) != value) and hasattr(self,
                                                       "change_%s" % name):
                getattr(self, "change_%s" % name)(req)
        self.flush()
        del self._prefs  # clear cache
        return req.redirect(self.url())

    update_prefs.permit = 'create page'

    def set_pref(self, pref, value):
        "updates a single pref in self.prefs - DOES NOT FLUSH"
        prefs = self.get_prefs()
        prefs[pref] = value
        self.prefs = ''
        for name, value in list(prefs.items()):
            self.prefs += '%s=%s\n' % (name, value)

###################### listings #########################

    @html
    def listing(self, req):
        ""

    def drafts_count(self, req):
        return self.count(isin={'kind': self.postkinds}, stage='draft')

    def drafts(self, req, pagemax=50):
        "draft items"
        limit = page(req, pagemax)
        req.pages = self.list(
            isin={'kind': self.postkinds},
            stage='draft',
            orderby="`when` desc,uid desc",
            limit=limit)
        req.title = 'drafts'
        req.page = 'drafts'  # for paging
        return self.listing(req)

#  def _latest(self,req,kinds="",order="`when` desc",where="",limit=50, first=False):

    def _latest(self, req, kinds="", order="`when` desc", where="", limit=50):
        " what's new? - based on lineage of the page, so page 1 gives everything"
        #    if first: #  a non-False value for first must be the number of items to show (this overrides limit)
        #     lim="0,%s" % first
        #    else:
        lim = page(req, limit) if limit else ""
        _kinds = kinds or self.postkinds
        _where = '%s%s lineage like "%s%%"' % (
            (where + " and ") if where else "", "rating>=0 and"
            if self.uid == 1 else "", self.lineage + str(self.uid) + '.')
        #print where
        #    items = self.list(isin={'stage':('posted','live'),'kind':_kinds},where=_where,orderby=order,limit=lim)
        items = self.list(
            stage='posted',
            isin={'kind': _kinds},
            where=_where,
            orderby=order,
            limit=lim)
        return items

    def latest(self, req):
        ""
        req.pages = self._latest(req)
        req.title = "latest"
        req.page = 'latest'  # for paging
        return self.listing(req)

    def latest_rss(self, req):
        if not has_rss:
            return 'rss support required'

        def escape(s):
            "work around the entification of & < >"
            s = s.replace("&", "[amp]")
            s = s.replace(">", "[gt]")
            s = s.replace("<", "[lt]")
            return s

        def unescape(s):
            "work around the entification of & < >"
            s = s.replace("[amp]", "&")
            s = s.replace("[gt]", ">")
            s = s.replace("[lt]", "<")
            return s

        items = [
            PyRSS2Gen.RSSItem(
                title=i.name,
                link="http://" + self.Config.domains[0] + i.url(),
                description=escape("<![CDATA[%s]]>" % i.text.formatted(req)),
                guid=PyRSS2Gen.Guid(i.url()),
                pubDate=i.when.datetime) for i in self._latest(req)
        ]
        rss = PyRSS2Gen.RSS2(
            self.name,
            "http://" + self.Config.domains[0] + self.url(),
            "recent activity for %s" % self.name,
            lastBuildDate=datetime.now(),
            items=items)
        outf = StringIO()
        rss.write_xml(outf, encoding='utf-8')
        outf.reset()
        res = unescape(outf.getvalue())
        return res

    feed = latest_rss

    #  @html
    #  def news_area(self,req):
    #    "wrappper-free news top 3"
    #    req.wrapper=None

    def news(self, req):
        """ what's new? (latest 3 items)
    call this from e.g. a wrapper with self.get(1).news(req)
    """
        where = '%s lineage like "%s%%"' % (
            self.uid == 1 and "rating>=0 and" or "",
            self.lineage + str(self.uid) + '.')
        req.pages = self.list(
            stage='posted',
            isin={'kind': self.postkinds},
            where=where,
            orderby="`when` desc",
            limit='0,3')
        req.title = "news"
        req.prep = 'from'
        req.wrapper = None
        return self.listing(req)

####################### search ########################

    @html
    def results(self, req):
        "search results"

    def search_extra_objects(self, term):
        "dummy to allow inheriting classes to insert other object results"
        return []

    def search(self, req):
        "search box supersearch"
        reslimit = 200  # we don't want more results than this...
        resleft = 0
        resfound = 0
        heads = []
        term = req.searchfor.upper().replace('"', '').replace("'", '').replace(
            '*', '%')
        # is it a uid?
        if safeint(term):
            try:
                heads = [self.get(safeint(term))]
            except:
                heads = []
        # search for matches..
        if len(term) > 2:
            req.searchfor = term  #store clean version
            # get title matches first
            heads.extend(
                self.list(
                    where='name like "%%%s%%"' % term, orderby='uid desc'))
            # now get text matches
            if len(heads) < reslimit:
                resleft = reslimit - len(heads)
                # get head uids
                head_uids = [a.uid for a in heads]
                # extensions
                bodies = [
                    p for p in self.search_extra_objects(term)
                    if p.uid not in head_uids
                ]
                resleft = resleft - len(bodies)
                # full text search of text bodies - remove any duplicates
                #            if resleft>0:
                bodies.extend(
                    self.list(
                        where=("match `text` against ('%s' in boolean mode)" %
                               term),
                        orderby="uid desc"))
                # we could limit the above to reslimit, but we don't know what is to be filtered out below....
                # note: "against('%s')" ignores any match that is in more than 50% of the rows
                #  - we get round this by using "against('%s' in boolean mode)
                # however, boolean mode does not sort results by relevance (hence the orderby clause is added also)
                # boolean mode: + and - operators indicate that a word is required to be present or absent, respectively,
                #  for a match to occur.
                # filter out duplicates
                bodies = [p for p in bodies if p.uid not in head_uids]
                # add together
                heads.extend(bodies)
            # filter out private items
            heads = self.visible(req.user, heads)
            resfound = len(heads)
            # cut to size
            heads = heads[:reslimit]
        # and display
        if len(heads) == 1:  #show result
            req.message = '1 result found for "%s"' % term
            #        return req.redirect(heads[0].url('view?searchfor=%s&message=%s' % (url_safe(term),url_safe(req.message))))
            return req.redirect(heads[0].url('view?searchfor=%s'))
#        return heads[0].view(req)
        if heads:
            req.message = '%s results found for "%s" %s' % (
                resfound, term,
                resfound > reslimit and ",first %s shown" % reslimit or "")
        else:
            req.warning = 'no results found matching "%s"' % term
        req.results = heads
        return self.get(1).results(req)
#  search.permit="guest"#allow anybody in

################ move / copy / export / import ################

    @html
    def import_form(self, req):
        ""
        pass

    import_form.permit = 'admin page'

    def export(self, req):
        "use redirect to allow a useful filename"
        return req.redirect(
            self.url('%s.%s.eve' % (self.Config.domain, self.uid)))

    export.permit = 'admin page'

    def export_eve(self, req):
        """exports a pickle of self and all descendents (ie branch)
       data files (images etc) are included (by get_branch(expand=True)) 
       user stub homepages are also included, so that authorship can be retained
       will only work for movekinds

       O/S - export requires local URLS to be replaced with external ones.. 
         - see old versions of TEXT.py

    """
        # get header info
        data = dict(
            version=self.Config.version,
            domain=self.Config.domain, )
        # get the branch, and prepare it (note that this trashes the data in self)
        branch = []
        for i in self.get_branch(expand=True):
            if i.stage != 'draft':  #exlude draft items ????????????????? DO WE WANT?NEED TO EXLUDE THEM ????
                i.text = i.text.exported(req)  # expand the links in the text
                branch.append(i)
        # export it all
        data.update(
            branch=[b.for_export(extras=['data']) for b in branch], )
        req.request.setHeader('content-type', 'application/octet-stream')
        return pickle.dumps(
            data,
            pickle.HIGHEST_PROTOCOL)  #pickle using highest protocol (binary)

    export_eve.permit = 'admin page'

    def import_eve(self, req):
        """imports a pickled branch and adds it as a child of self"
    """
        if not req.filename:
            return self.import_form(req)
        # fetch the data
        try:
            data = pickle.loads(req.filedata)
        except:
            raise
            req.error = 'cannot import "%s"' % req.filename
            return self.import_form(req)
        # convert from export dict format to objects
        branch = [self.get(0, data=i) for i in data['branch']]
        # convert and store the branch
        return self.store_branch(req, branch)

    import_eve.permit = 'admin page'

    def store_branch(self, req, branch):
        "converts branch to be a child of self, and adds it to the database"
        # fix the descendents
        for ob in branch:
            #      print ">>>>>>>>>>>>>>> ob=",ob.__dict__
            #      ob.table=self.table # fix the table so we have the correct database!
            #      ob.Config=self.Config # fix Config
            #      print "content ",ob.uid,ob.kind,ob.name,ob._v_changed
            nob = self.new()
            for i in branch:  # fix the parent of any child
                if i.parent == ob.uid:
                    i.parent = nob.uid
            ob.uid = nob.uid
            if hasattr(ob, 'data'):  # store file data
                if ob.kind == 'image':
                    ob.code = '%s.%s' % (ob.uid, ob.code.split(".")[-1]
                                         )  # rename image files to use new uid
                ob.save_file(ob.data)
            ob.all_change()
            #      print "flushing ",ob.uid,ob.kind,ob.name,ob._v_changed
            ob.flush()
        # here it
        req._import = self.get(
            branch[0].uid
        )  # get the local object (ie not the imported one, which won't work in here())
        return self.here(req)

    def move(self, req):
        "marks page for moving (stored in user cache)"
        if not req.user.can('admin page'):
            return self.view(req)
        self.set_move(req)
        return req.redirect(
            self.url('view?message=%s' % url_safe(
                'page %s marked for moving - now navigate to the required destination'
                % self.uid)))

    move.permit = 'create page'

    def copy(self, req):
        "duplicate self and all descendents (ie branch) - will only work for movekinds"
        move = self.get_move(req)
        if move:
            req._copying = True
            return self.store_branch(req, move.get_branch(expand=True))
        req.warning = 'system was reset - page copy canceled'
        return self.view(req)

    copy.permit = 'create page'

    def cancel_move(self, req):
        "clear the session cache move uid"
        req.cache['page_move'] = None
        message = 'page move cancelled'
        return self.view(req)

    def here(self, req):
        "moves marked page here (as a child)"
        move = req._import or self.get_move(req)
        if move:
            # fix parent, lineage
            move.parent = self.uid
            move.set_lineage(self)
            move.set_descendant_lineage()
            move.flush()
            message = '"%s" %s here' % (
                move.get_name(), (req._copying and 'copied') or
                (req._import and 'imported') or 'moved')
            req.cache['page_move'] = None  # clear the session cache move uid
        else:
            req.warning = 'system was reset - page move canceled'
            return self.view(req)
        return req.redirect(self.url('view?message=%s' % url_safe(message)))

    here.permit = 'create page'

    def set_move(self, req):
        "stores self.uid in session cache (req.cache['page_move'])"
        req.cache['page_move'] = self.uid

    @classmethod
    def get_move(cls, req):
        "gets move uid from session cache (req.cache['page_move'])"
        move = req.cache.get('page_move', None)
        #    print ">>>>>>>>>> move=",move
        if move:
            if cls.exists(move):
                return cls.get(move)
        return None

##### shortcuts

    def login(self, req):
        "shortcut to user login"
        req.return_to = self.url()
        return req.user.login_form(req)

    login.permit = "guest"

    ################ templates for mix-in classes ##############
    #
    # These are here for now, as evoke.render.html() uses the last item in the module name to
    #  obtain the template name...
    # Note that the __module__ for an @html function declared in Image.py will be "Page.Image",
    #  so it would be possible to derive "Page" from that....

    @html
    def file_add(self, req):
        ""
        req.page = "add_file"  # for tab display

    file_add.permit = 'edit page'

    @html
    def image_add(self, req):
        ""
        req.page = 'add_image'  # for tab display

    image_add.permit = 'edit page'

    ################ utilities ##################

    # map old cells layout to Bootstrap
    def cell_to_col(self, cell):
        "map cell id in form <cells-per-row><position> to Bootstrap col-md-<cells>"
        cell = int(str(cell)[0])
        # becomes less precise where cell=5 and cell>7
        cell_col = {1: 12, 2: 6, 3: 4, 4: 3, 5: 2, 6: 2}
        return cell_col.get(cell, 1)

    def list_prefs(self, req):
        "lists prefs for self"
        prefs = self.get_prefs()
        o = self.prefs + '<br/><br/>'
        for i in prefs:
            o += '%s: %s<br/>' % (i, self.get_pref(i))
        if req.pref:
            o += '%s: %s' % (req.pref, str(self.get_pref(req.pref)))
        return o

    def list_config(self, req):
        "lists self.Config settings"
        o = ''
        for i in sorted(self.Config.__dict__.items()):
            if i[0] == 'connect':
                i = (i[0], '*************')
            o += '<b>%s</b> : %s  <br/>' % i
        return o

    list_config.permit = 'admin page'

    def list_items(self, req):
        "lists self's items"
        o = ''
        for i in sorted(self.__dict__.items()):
            o += '<b>%s</b> : %s  <br/>' % i
        return o

    list_items.permit = 'admin page'

    def info(self, req):
        ""
        o = []
        o.append("<b>module:</b> " + self.__class__.__module__)
        o.append("<b>class:</b> " + self.__class__.__name__)
        o.append("<b>bases:</b> " + ','.join(
            (b.__name__ for b in self.__class__.__bases__)))
        o.append("<b>dict:</b>")
        for (k, v) in list(self.__dict__.items()):
            o.append('<i>%s:</i> %s' % (k, str(v)))
        o.append("<b>class dict:</b>")
        for (k, v) in list(self.__class__.__dict__.items()):
            o.append('<i>%s:</i> %s' % (k, str(v)))
        return '<br/>'.join(o)

    info.permit = 'admin page'

    def delf(self, req):
        "removes superfluous line ends from text - e.g. emailed text"
        self.text = delf(self.text)
        self.flush()
        req.message = "line ends removed"
        return self.edit(req)

    delf.permit = 'admin page'

    ################# FIXES ########################

    def fix_seq(self, req):
        "resets seq for current page"
        self.set_seq()
        self.flush()
        req.message = 'seq fixed'
        return self.view(req)

    fix_seq.permit = "admin page"

    def fix_lineage(cls, req, ret=True):
        'set lineage throughout'
        s = cls.get(2)
        s.parent = 1
        s.flush()
        s = cls.get(1)
        s.lineage = "."
        s.flush()
        s.set_descendant_lineage()
        if ret:
            req.message = 'lineage fixed!'
            return cls.get(1).view(req)

    fix_lineage.permit = 'admin page'
    fix_lineage = classmethod(fix_lineage)

    def testvar(cls, req):
        req.message = cls.Var.say('version')
        return cls.latest(req)

    def testbug(self, req):
        ""
        x = 1 + 'three'
        return self.view(req)

    ############## ONE-OFF FIXES ##################

#    def to_md(self,req):
#      ""
#      n=0
#      for i in self.list():
#        if i.text:
#          i.text=i.text.to_markdown(req)
#          i.flush()
#          n+=1
#      req.message='%s pages converted to md' % n
#      return self.view(req)

