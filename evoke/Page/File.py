""" 
Files - a Page kind - used in pages/sections, as download links

mix-in class for Page.py
"""

from evoke.render import html
from evoke.lib import *

from io import StringIO
from os.path import lexists
import os


class File(object):
    ""

    #  def permitted(self,user):
    #    ""
    ##    print ">>>>>>>>>>>>>>>>>>>>>>>", self.get_pob().permitted(user)
    #    return self.get_pob().permitted(user)

    #  def get_page(self):
    #    ""
    #    return self.get(self.uid)

    #  def get_pob(self):
    #    "parent Page object - cached for efficiency - assumes details wont change during lifespan of instance"
    #    if not hasattr(self,"pob"):
    #      self.pob=self.get(self.parent)
    #    return self.pob

    def file_url(self):
        "relative url"
        return '/site/data/%s/%s' % (self.file_folder(), self.code)

    def edit_file(self, req):
        "file edit"
        req.page = "edit"
        if req.name:  #we have an update
            self.name = req.name
            if req.filedata:  # replace filedata
                if not self.file_saved(
                        req, replace=True):  # save new file under existing uid
                    return self.edit_form(req)  # give error
            if req.code and req.code != self.code:  # rename the file
                req.code = req.code.replace(' ', '_')  #fix spaces
                os.rename(
                    self.file_loc(),
                    self.file_loc(name=req.code))  #rename the physical file
                req.message = req.message or 'file renamed to "%s"' % req.code
                self.code = req.code
            # store changes
            self.flush()
            return self.get_pob().redirect(req, '')
        return self.get_pob().redirect(req, 'add_file')

    edit_file.permit = 'edit page'

    def add_file(self, req):
        ""
        _file = self.file_saved(req)
        if hasattr(self, 'files'):
            del self.files  # clear cache
        return self.file_add(req)

    add_file.permit = 'edit page'

    def file_loc(self, name=''):
        "file location"
        return '%sdata/%s/%s' % (self.Config.site_filepath,
                                      self.file_folder(), name or self.code)

    def file_folder(self):
        """folder (within data folder) for a file:
    - this can be overridden by apps
    based on uid:
    - returns "123/456" for uid 123456789, so filepath will be 123/456/filename.ext
    - this allows us to exceed a billion files.... (or more precisely a billion pages)
    - if it over-rides, eg 1,234,567,890 we get 1234/567/filename.ext, so it doesn't immediately break...."""
        s = "%09d" % self.uid
        return "%s/%s" % (s[:-6], s[-6:-3])

    def filedata(self, folder='', name=''):
        "return file data as bytes - generic, used also for images"
        d = '%sdata/%s' % (self.Config.site_filepath,
                                folder or self.file_folder())
        fp = '%s/%s' % (d, name or self.code)
        try:
            f = open(fp, 'rb')
            return f.read()
        except:
            return ''

    def save_file(self, content, folder="", name=''):
        "save actual file in filesystem - generic, used also for images"
        folders = folder or self.file_folder()
        datapath = '%sdata' % self.Config.site_filepath
        fp = '%s/%s/%s' % (datapath, folders, name or self.code)
        try:
            f = open(fp, 'wb')
        except:  # folder not found...
            d = '%s/%s' % (datapath, folders)
            os.makedirs(d)
            #      os.makedirs(d,02777) # doesn't work reliably
            #     so, fix permits
            d = datapath
            for folder in folders.split('/'):
                d = "%s/%s" % (d, folder)  #append folder
                os.chmod(d, 0o2777)
            try:    
                f = open(fp, 'wb')
            except:
                print(self, datapath, folders, name, self.code)
        os.chmod(fp, 0o2664)
        f.write(content)

    def file_saved(self, req, replace=False):
        """if req is valid, stores new file and returns Page object, else returns 0. 
    Folder can optionally be specified in req.folder
    if replace, then self should be a file page object and will be re-used 
    """
        if 'filedata' in req:
            filedata = req.filedata
            extension = req.filename.split(".")[-1].lower()
            name = req.filename.replace('\\', '/').split('/')[
                -1]  # fix MS brain-dead slashes, and strip off path
            #      print ">>>>>>>>>>>>>>>>>>>>",req.filename,extension
            if not filedata:
                req.error = "please provide a file"
            if not req.error:
                if replace:
                    self.delete_file(
                        replace=True)  # delete old file (keep self)
                    _file = self
                else:  # create new file page
                    _file = self.new()
                    _file.parent = self.uid
                    _file.kind = 'file'
                    _file.seq = req.seq or 0xFFFFFF  # place at end of siblings
                    _file.name = name
                    _file.stage = 'live'
                    _file.update(req)
                    _file.set_lineage()
                _file.code = name.replace(' ', '_')
                _file.when = DATE()
                # store file page
                _file.flush()
                if not replace:
                    _file.renumber_siblings_by_kind()  #keep them in order
                # save the file
                _file.save_file(filedata, req.folder)
                # return
                req.message = 'file "%s" %s' % (
                    _file.name, replace and "replaced" or "added")
                return _file
        return None

    def get_files(self):
        "set self.files, i.e. a list of all child file objecs, if it is not already set, and returns it"
        if not hasattr(self, 'files'):
            self.files = self.list(
                parent=self.uid, kind="file", orderby='seq,uid')
        return self.files

    def delete_file(self, replace=False):
        "delete the file, and (unless replace is True) the file page"
        if not replace:
            self.delete()  #delete the file page
        try:
            os.remove(self.file_loc())  #delete the file
        except:
            pass  # delete failed and we don't care :)

    def delete_files(self):
        "delete all child files"
        for i in self.get_files():
            i.delete_file()

    def remove_file(self, req):
        "delete file"
        self.delete_file()
        self.renumber_siblings_by_kind()
        return self.get_pob().redirect(req, 'add_file')

    remove_file.permit = 'edit page'
