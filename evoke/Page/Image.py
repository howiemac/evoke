"""
Images - a Page kind

mix-in class for Page.py
"""

from io import BytesIO
from os.path import lexists
import imghdr

from PIL import Image as pim

from evoke.render import html
from evoke.lib import *


class ImageError(Exception):
    """Base class for exceptions in this module."""
    pass


class Image(object):
    ""

    def autostyle(self,scale=100):
        "return a CSS style to autosize the image for use in evo template"
        d = self.get_stage_data()
        w = safeint(d["full_width"])
        h = safeint(d["full_height"])
        if (w and h) and (w > h):
            return "height:%d%%" % (h * scale // w,)
        else:
            return "height:%s%%" % scale

    def get_pos(self):
        "pos is first word in self.stage"
        return self.get_stage_data()['pos']

    def get_size(self):
        "size is second word in self.stage"
        return self.get_stage_data()['size']

    def get_width(self, size=''):
        "display width for chosen size"
        d = self.get_stage_data()
        w = d['%s_width' % (size or d['size'], )]
        if w == '?':
            w = safeint(
                self.get_pref('thumb_size')
            )  #bodge to allow for broken images which refuse to thumnail
        return w

    def get_display(self):
        "returns data in display format as a list of lines"
        dat = self.stage.split()
        full = dat[1] == 'full'
        d = ['%s %s %s' % (dat[0], dat[1], full and dat[2] or dat[3])]
        if full:
            d.append('thumb %s' % dat[3])
        return d

    def get_stage_data(self):
        """unravels the data in stage and returns it as a dictionary (Cached)
    {
    'pos':'left'|'center'|'right'|'hidden'
    'size':'full'|'thumb'
    'full_width': ?px # can be custom resized for display only (original file stays unaltered)
    'full_height': ?px
    'thumb_width': ?px # can be custom resized (thumbnail file is replaced)
    'thumb_height': ?px
     }
    Also updates stage data from file headers where required, and massages legacy data
    NOTE: heights are not currently used.... (19/6/8)
        : an extra thumbdefault_ height/width pair was stored up to version 2899 - now obsolete 
    """
        if not hasattr(self, '_stage_data'):
            data = self.stage.split()
            # add missing data (for legacy images, and newly created images)
            save = False
            if len(data) == 1:
                data.append('full')
                save = True
            if len(data) == 2:
                if lexists(self.file_loc()):
                    data.append(
                        '%sx%s' % pim.open(open(self.file_loc(), 'rb')).size)
                else:
                    data.append('?x?')
                save = True
            elif data[2][0] == 'x':
                if lexists(self.file_loc()):
                    data[2] = '%sx%s' % pim.open(
                        open(self.file_loc(), 'rb')).size
                else:
                    data[2] = '?x?'
                save = True
            if len(data) == 3:
                if lexists(self.thumb_loc()):
                    data.append(
                        '%sx%s' % pim.open(open(self.thumb_loc(), 'rb')).size)
                else:
                    data.append('?x?')
                save = True
            elif data[3][0] == 'x':
                if lexists(self.thumb_loc()):
                    data[3] = '%sx%s' % pim.open(
                        open(self.thumb_loc(), 'rb')).size
                else:
                    data[3] = '?x?'
                save = True
#      if len(data)==4:
#        if lexists(self.thumb_loc()):
#          data.append('%sx%s' % pim.open(open(self.thumb_loc())).size)
#        else:
#          data.append('?x?')
#       save=True
# convert the data to a dictionary
            datadict = {
                'pos': data[0],
                'size': data[1],
                'full_width': data[2].split('x')[0],
                'full_height': data[2].split('x')[1],
                'thumb_width': data[3].split('x')[0],
                'thumb_height': data[3].split('x')[1]
            }
            if save:
                self.set_stage(datadict)
                self.flush()
            else:
                self._stage_data = datadict
        return self._stage_data

    def set_stage(self, data):
        "bundles sundry display data, in format 'pos size wxh thumb_wxh into the stage field - DOES NOT FLUSH"
        self.stage = "%(pos)s %(size)s %(full_width)sx%(full_height)s %(thumb_width)sx%(thumb_height)s" % data
        self._stage_data = data  # update cache
        return self.stage

    def get_anchor_width_style(self, size=''):
        "anchor width style for images"
        style = ''
        if self.get_pos() in ('left', 'right', 'center'):
            style = ('width:%spx' % self.get_width(size=size))
        return style

    def get_image_width_style(self, size=''):
        "img width style for images"
        style = ('width:%spx' % self.get_width(size=size))
        return style

    def update_image(self, req):
        ""
        #    if (req.url!=  # create / maintain link
        self.update(req)
        self.flush()

#  @html
#  def view(self,req):
#    "image view"
#    req.page='view'

    def edit_image(self, req):
        "image edit handler"
        req.page = 'edit'
        if req.pos:  #we have an update
            # replace filedata
            if req.filedata:
                if not self.image_saved(
                        req,
                        replace=True):  # save new image under existing uid
                    return self.edit_form(req)  # give error
                req.width = ''  # we don't want the old width
            # update stage data
            data = self.get_stage_data()
            data['pos'] = req.pos
            if not req.width:
                #       if data['%s_height' % req.size]=='?':
                data[
                    '%s_width' % req.
                    size] = ''  #this will force a re-fetch in get_stage_data()
            elif req.width != self.get_width(size=req.size):
                data['%s_width' % req.size] = req.width
                data['%s_height' % req.size] = '?'
                if req.size == 'thumb':
                    self.make_thumb(width=safeint(req.width))
            data['size'] = req.size
            req.stage = self.set_stage(data)
            # store changes
            self.update_image(req)
            return self.get_pob().redirect(req, '')
        return self.get_pob().redirect(req, 'add_image')

    edit_image.permit = 'edit page'

    @classmethod
    def convert_image(self, filedata):
        "assumes filedata is BMP or some other compatible format - converts to JPG"
        bmp = BytesIO(filedata)
        bmp.reset()
        img = pim.open(bmp)
        f = BytesIO()
        img.convert('RGB').save(f, 'JPEG')
        f.reset()
        return f.read()

# thumbnails #########################################

    def thumb_url(self):
        """url for a jpeg thumbnail of the image 
       - makes the thumbnail on the fly, if necessary
       - currently makes thumnail copies even for small files, regardless
       - thumbnail size is determined by self.get_pref('thumb_size')
    """
        if not lexists(self.thumb_loc()):
            try:
                self.make_thumb()
            except ImageError:
                #raise
                print("**** CANNOT THUMBNAIL:", self.file_loc())
                return self.file_url()
            except:
                return ''
        return '/site/data/%s/%s' % (self.thumb_folder(), self.thumb_name())

    def thumb_loc(self):
        "location of jpeg thumbnail of the image - the thumbnail may not exist"
        return '%sdata/%s/%s' % (self.Config.site_filepath,
                                      self.thumb_folder(), self.thumb_name())

    def thumb_folder(self):
        "folder (within data folder) for thumbnails - this can be overridden by apps"
        return self.file_folder() + '/thumbs'

    def thumb_name(self):
        "filename for a jpeg thumbnail of the image - the thumbnail may not exist"
        #    return "%s.%s" % (self.uid,self.get_extension())
        return "%s.jpg" % self.uid

#  def get_extension(self):
#    "JPEG, GIF, or PNG"
#    return self.code.split(".")[-1].lower()

    def make_thumb(self, width=0):
        "creates and stores a jpeg thumbnail of the image"
        #    print "making thumb for: ",self.uid
        im = open(self.file_loc(), 'rb')
        x = pim.open(im)
        #    print 'x.size:',x.size[1], x.size[0]
        size = width or safeint(self.get_pref('thumb_size'))
        #    print ">>>>>>>>>>>>>>>>>>>>> creating thumb:",size
        #    print ">>>>>>>>>>>>>>>>>>>>> thumb_size:",self.get_pref('thumb_size')
        #    print ">>>>>>>>>>>>>>>>>>",size, x.size[1], x.size[0]
        try:
            x.thumbnail((size, size * x.size[1] / x.size[0]), pim.ANTIALIAS)
        except Exception as e:
            print("ERROR MAKING THUMBNAIL for %s:%s" % (self.uid, e))
        f = BytesIO()
        #    extension=self.get_extension().upper()
        #    x.convert(extension!='GIF' and 'RGB' or None).save(f,extension) #this creates thumbs of correct type but the non-jpg ones can be much bigger than the originals!
        x.convert('RGB').save(f, 'JPEG')
        f.seek(0)  # back to the start..
        self.save_file(f.read(), self.thumb_folder(), self.thumb_name())
        # update thumb width and height in stage
        data = self.get_stage_data()
        data['thumb_width'], data['thumb_height'] = x.size
        self.set_stage(data)
        self.flush()

    def test_thumb(self, req):
        self.get_thumb()
        req.message = 'thumbnail saved'
        return self.view(req)

    def image_or_thumb_url(self):
        "display according to stage size"
        return "thumb" in self.stage and self.thumb_url() or self.file_url()

#### add / remove an image #######

    def add_image(self, req):
        ""
        image = self.image_saved(req)
        if hasattr(self, 'images'):
            del self.images  # clear cache
        if image:
            return self.redirect(req, "")
        return self.image_add(req)

    add_image.permit = 'edit page'

    def image_saved(self, req, replace=False):
        """if req is valid, stores new image and returns image object, else returns 0. 
    Folder can optionally be specified in req.folder
    Requires req.filedata and req.filename
    if replace, then self should be an image page object and will be re-used 
    """
        if 'filedata' in req:
            filedata = req.filedata
            extension = (imghdr.what('', filedata)
                         or req.filename.split(".")[-1].lower()).replace(
                             'jpeg', 'jpg')
            if not filedata:
                req.error = "please provide an image file"
            elif extension == 'bmp':
                filedata = self.convert(filedata)
                extension = 'jpg'
            elif extension not in ('gif', 'png', 'jpg', 'jpeg'):
                req.error = "only JPEG, GIF, PNG, and BMP are supported - please select another image"
            if not req.error:  # check that size is not too big
                if len(filedata) > 1 << 20:
                    req.error = "image size exceeds 1 megabyte - please use a smaller image"
            if not req.error:
                if replace:
                    self.delete_image(
                        replace=True)  # delete old file (keep self)
                    image = self
                    image.stage = '%s %s' % (self.get_pos(), self.get_size(
                    ))  # reset stage to remove sizing data from old image
                    del image._stage_data  #clear the cache
                else:  # create a new image page
                    image = self.new()
                    image.parent = self.uid
                    image.kind = 'image'
                    image.seq = req.seq or 0xFFFFFF  #place at end of siblings
                    # set default size (same as last sibling)
                    size = self.get_images() and (
                        len(self.images) >
                        0) and self.images[-1].get_size() or 'full'
                    image.stage = 'right %s' % size  #rest of stage data will be added on the fly later by get_stage_data()
                    image.update(req)
                    image.set_lineage()
                image.text = req.filename.replace('\\', '/').split(
                    '/'
                )[-1]  #store source filename for reference (fix MS brain-dead slashes, and strip off path)
                image.code = "%s.%s" % (image.uid, extension.lower())
                image.when = DATE()
                image.flush()  #store the image page
                if not replace:
                    image.renumber_siblings_by_kind()  #keep them in order
                # save the image file
                image.save_file(filedata, req.folder)
                # return
                req.message = 'image "%s" %s' % (
                    image.text, replace and
                    "replaced: refresh your browser cache to view the replacement"
                    or "added")
                return image
        return None

    def get_images(self):
        "sets self.images, i.e. child images (up to a maximum of 50 images), and returns it"
        if not hasattr(self, 'images'):
            if self.kind == "image":
                self.images = [self]
            else:
                self.images = self.list(
                    parent=self.uid,
                    kind="image",
                    orderby='seq,uid',
                    limit="50")
        return self.images

    def get_image(self):
        "return first image only, from get_images()"
        images = self.get_images()
        return images and images[0] or None

    def delete_image(self, replace=False):
        "delete the image file and thumbnail, and (unless replace) the image page"
        self.delete_file(replace)
        try:  # this may not exist, so don't raise an error
            os.remove(self.thumb_loc())  #delete the thumbnail
        except:
            pass

    def delete_images(self):
        "delete all child images"
        for i in self.get_images():
            i.delete_image()

    def remove_image(self, req):
        "delete image"
        self.delete_image()
        self.renumber_siblings_by_kind()
        return self.get_pob().redirect(req, "add_image")

    remove_image.permit = 'edit page'
