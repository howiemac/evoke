""" evoke html generation:

new age templating for evoke

consists of 2 code files:
- evo.py : template parser
- html.py : decorator for template-calling-functions (ie python interface)

a 2-pass process:
1: evo=>pyc  (done only once per template, unless the template is altered)
  - parse the source template into python
  - compile the python into byte code
  - cache it
2: pyc=>html (each call ie on the fly)
  - eval the python in the current context

allows for multiple apps having different versions of same-name templates

issues:
- let only creates variables in the local context (albeit using globals()!?!) - any include does not see them.....
- let only allows simple variables: would be nice to allow "instance.var=whatever"
- let and include blocks have the indentation of the definition context, not the use context - more hassle to fix than it is worth?
- cannot use += for local variables, eg count+=1 (would be nice..)
- better error pinpointing would be nice
"""
debug = False  #slows things down considerably, and is more verbose...
#debug=True
sparse = True  #keep the output lean? ie no indents or comments

from os import stat
from os.path import lexists
import sys
from gettext import gettext

from evoke import lib

# EVO error handling


class EvoError(lib.Error):
    "EVO ERROR"


class EvoTemplateNotFound(EvoError):
    'template "%s" not found'


class EvoParseError(EvoError):
    '%s on line %s in %s : %s'


class EvoSyntaxError(EvoError):
    'line %s in %s : %s ^ %s'


# Evo() parser,compiler, and renderer


class EvoCacheObject(object):
    "stores python code and timestamp"

    def __init__(self):
        self.pyc = ""


class Evo(object):
    "an evoke template"

    pycache = {}
    pathcache = {}

    def __init__(self, filename):
        "initialise Evo instance - allow for absence of gettext."
        self.filename = filename

    def __call__(self, ob, req, wrapper='', gettext=lambda x: x):
        """ generate HTML
    - allow for multiple apps having different template versions within each Evo instance - we ony get the app data at call time
    - wrapper can be passed as req.wrapper or as wrapper: wrapper=None means no wrapper
    """
        self.gettext = gettext
        #get the template path for this app
        self.key = ob.Config.app + '.' + self.filename
        self.path = self.pathcache.get(self.key, "")
        if not self.path:  # first time only.. get the paths
            # note: we couldn't do this earlier, as we lacked the evoke and app filepaths
            # firstly: use the local class template, if there is one
            klass = self.filename.split("_", 1)[0]
            self.path = '%s%s/evo/%s' % (ob.Config.app_filepath, klass,
                                         self.filename)
            # otherwise, is there a local template?
            if not lexists(self.path):
                self.path = ob.Config.app_filepath + 'evo/' + self.filename
                # otherwise, is there a class template in evoke (base)?
                if not lexists(self.path):
                    self.path = '%s%s/evo/%s' % (ob.Config.evoke_filepath,
                                                 klass, self.filename)
                    # otherwise, use the evoke template
                    if not lexists(self.path):
                        self.path = ob.Config.evoke_filepath + 'evo/' + self.filename
                        # if that doesn't exist, raise an error
                        if not lexists(self.path):
                            raise EvoTemplateNotFound(self.filename)
            self.pathcache[self.key] = self.path
        #get the CacheObject for this path, containing the python code and timestamp
        cob = self.pycache.get(self.path, EvoCacheObject())
        #and parse the template to a python expression
        # ... unless we already have python code and the timestamp matches
        if (not cob.pyc) or (cob.timestamp != stat(self.path).st_mtime):
            # set the timestamp
            cob.timestamp = stat(self.path).st_mtime
            # parse the template into python code
            cob.pyc = self.parse(self.path, ob, req)
            # compile the python code to the cache
            #      print("compiling %s : " % self.filename,cob.pyc)
            if not debug:
                try:
                    cob.pyc = compile(cob.pyc, '<string>', 'eval')
                except SyntaxError as inst:
                    p = inst.offset
                    t = inst.text
                    raise EvoSyntaxError(
                        "char %s" % p, "evo pycode for %s" % self.filename,
                        t[max(0, p - 40):p], t[p:min(p + 40, len(t) - 1)])
        # sort out the wrapper
        if wrapper is not None:
            wrapper = wrapper or req.get('wrapper', 'wrapper.evo')
        # and run the python code from the cache
        res = self.wrap(cob.pyc, ob, req,
                        wrapper) if wrapper else self.evaluate(
                            cob.pyc, ob, req)
        return res

    def evaluate(self, pyc, ob, req):
        "run the python code, returning the result"
        ns = dict(namespace)

        #mygettext = getattr(self, 'gettext', gettext)
        mygettext = req.gettext

        ns.update({'req': req, 'self': ob, 'lib': lib, '_': mygettext})
        try:
            return eval(pyc, ns, {})
        except SyntaxError as inst:
            p = inst.offset
            t = inst.text
            #      lib.send_error(inst, sys.exc_info())
            raise EvoSyntaxError("char %s" % p, 'evo bytecode',
                                 t[max(0, p - 40):p],
                                 t[p:min(p + 40, len(t) - 1)])
        except Exception as inst:
            #      lib.send_error(inst, sys.exc_info())
            raise

    def wrap(self, pyc, ob, req, wrapper):
        ""
        ob._v_content_pyc = pyc
        return Evo(wrapper)(ob, req, wrapper=None) or ' '

    def parse(self, codefile, ob, req):
        "evo => python parser "
        stack = []

        def pop(stack=stack):
            x = stack[-1]
            del stack[-1]
            return x

        def push(x, indent, kind='tag', stack=stack):
            stack.append((x, indent, kind))

        # fetch the code from the file, and de-tab it
        code = open(codefile, 'r').read().expandtabs(8)
        # use a list to gather the output
        output = []
        # put() will be our workhorse for appending output
        put = output.append
        # init the other vars we will need..
        indent = 0
        line = 0
        cont = ''
        # start parsing
        for i in [l.rstrip() for l in code.split("\n")] + ["end.of.file"]:
            line += 1
            i = cont + i
            if i.endswith('\\'):  # handle continuation
                cont = i[:-1]
                continue
            cont = ''
            x = i.lstrip()
            if x and not x.startswith('#'):  #ignore comments
                xindent = indent
                if x == "end.of.file":
                    indent = stack[0][1]  #dedent to first indent
                    x = ''
                else:
                    indent = len(i) - len(x)
                if stack and indent <= xindent:
                    while True:
                        xxindent = xindent
                        e, xindent, kind = pop()
                        if (e is not None):
                            if kind == 'logic':
                                put(e)
                            else:
                                c = (e and output[-1][-1] != '(') and ',' or ''
                                z = '%s%s).out(%s)' % (c, e, len(stack))
                                put(z)
                        if stack and stack[0][1] != 0:
                            raise EvoParseError("STACK WRONG", line, self.path,
                                                x)
#              print("STACK WRONG line %s in %s : %s" % (line,self.path,x), stack)
                        if indent >= xindent or not stack:
                            if indent > xindent:
                                raise EvoParseError("INVALID DEDENT", line,
                                                    self.path, x)
#              print("INVALID DEDENT line %s in %s : %s" % (line,self.path,x), xindent,indent)
                            elif not stack and indent < xindent:
                                raise EvoParseError("STACK EMPTY", line,
                                                    self.path, x)
#              print("STACK EMPTY line %s in %s : %s" % (line,self.path,x), xindent,indent)
                            break
                if x:
                    kind = 'logic'
                    if x.startswith("'") or x.startswith('"'):  #raw text
                        s = x
                        e = None
                        kind = 'raw'
                    elif x.startswith('('):  #expression
                        s = x
                        e = None
                        kind = 'expr'
                    elif x.find(":") > -1:
                        s, e = x.split(":", 1)
                        if s[:4] == 'for ':
                            e = ') %s))' % s
                            s = '("\\n".join(('
                        elif s[:3] == 'if ':
                            e = ') or " ")'  #this makes sure that the whole if-elif-else clause is True
                            s = '((%s) and (' % s[3:]
                        elif s[:4] == 'elif':
                            e = output[-1] + ')'
                            del output[
                                -1]  #drop the end from the previous if or elif
                            xindent = xxindent  #and reset xindent
                            s = ' or " ") or ((%s) and (' % s[5:]
                        elif s[:4] == 'else':
                            e = output[-1]
                            del output[
                                -1]  #drop the end from the previous if or elif
                            xindent = xxindent  #and reset xindent
                            s = ' or " ") or ('
                        else:  #standard tag
                            s += '('
                            kind = 'tag'
                    elif x.find("=") > -1:  #we have a let
                        s, e = x.split("=", 1)
                        # allow for whitespace in let lines
                        s = s.strip()
                        e = e.strip()
                        #error check
                        f = s.find(' ')
                        if f > -1:
                            raise EvoSyntaxError(line, self.path, x[:f], x[f:])
                        #set s and e
                        s = 'let(globals(),%s=' % s
                        e = '%s)' % e
                    elif x.endswith('.evo'):
                        parts = x.rsplit('.', 2)
                        s = 'include("%s.evo",%s,req)' % (
                            parts[-2], len(parts) == 3 and parts[0] or 'self')
                        e = None
                        kind = 'include'
                    elif x == 'content.here':  #wrapper include
                        s = 'content(self,req)'
                        e = None
                        kind = 'include'
                    else:
                        s = x
                        e = None
                        kind = 'expr'
                    if kind == 'expr':  #check syntax
                        try:  #this will break if there are any globals - we are just looking for syntax errors
                            eval(x)
                        except SyntaxError as inst:
                            # send_error(inst, sys.exc_info())
                            raise EvoSyntaxError(line, self.path,
                                                 x[:inst.offset],
                                                 x[inst.offset:])
                        except:  #ok
                            pass
                    push(e, indent, kind)
                    z = '%s%s' % ("+'\\n'+" if
                                  (output and (xindent == indent)) else "", s)
                    put(z)
        # annotate the output with codefile start/stop messages
        if output and not sparse:
            if output[0].find("<!DOCTYPE") >= 0:
                output.insert(1, "+'\\n<!-- start of %s -->\\n'" % codefile)
            else:
                output.insert(0, "'\\n<!-- start of %s -->\\n'+" % codefile)
            put("+'\\n<!-- end of %s -->\\n'" % codefile)
        # glue together and return
        py = "".join(output)
        if debug:
            print("PYTHON:", py)
        return py


#################################################################
# evo template language definition: support classes and functions
#################################################################


def tag(name, _base='', _clean=False, **defaults):
    "class factory to make tag classes"
    singleton = name[-1] == "/" and "/"
    if singleton:
        name = name[:-1]
    self = type(name, (_base or basetag, ), defaults)
    self.defaults = dict(defaults)
    if _clean:
        self.template = "<%s%s%s>" % (name, "%(attributes)s",
                                      singleton or (">%s</%s" %
                                                    ("%(content)s", name)))
    elif sparse:
        self.template = "<%s%s%s>" % (name, "%(attributes)s",
                                      singleton or (">\n%s\n</%s" %
                                                    ("%(content)s", name)))
    else:
        self.template = "%s<%s%s%s>%s" % ("%(indent)s", name, "%(attributes)s",
                                          singleton or (">\n%s\n%s</%s" %
                                                        ("%(content)s",
                                                         "%(indent)s", name)),
                                          "%(_comment)s")
    return self


class basetag(object):
    "foundation class for tag classes"

    def __init__(self, _content="", **attributes):
        self.content = _content
        self.attributes = dict(self.defaults)  #separate copy
        self.attributes.update(
            attributes)  #now, attributes contains everything we need
#    print "ATTRIBUTES:",self.attributes

    keymap = {
        'cls': 'class',
        'equiv': 'http-equiv',
        'for_id': 'for',
    }

    def out(self, indent=0):
        def subst(k):
            "substitute attribute name"
            if k in self.keymap:
                return self.keymap[k]
            # convert _ to -
            return k.replace('_', '-')

        attributes = "".join([
            (' %s="%s"' % (subst(k), str(v).replace('"', "&quot;")))
            for (k, v) in self.attributes.items()
            if v or (k not in ('checked', 'selected', 'disabled', 'itemscope'))
        ])
        return self.template % dict(
            indent=" " * indent,
            attributes=attributes,
            content=self.content,
            _comment=not sparse and 'id' in self.attributes and
            (" <!-- end of %s -->" % self.attributes.get('id')) or "")


#HTML tags
a = tag('a')
article = tag('article')
b = tag('b')
big = tag('big')
body = tag("body")
br = tag("br/")
button = tag('button', type='submit')
#caption=tag('caption')
center = tag('center')  # deprecated - use CSS
cite = tag('cite')
dd = tag('dd')
dl = tag('dl')
dt = tag('dt')
div = tag("div")
fieldset = tag('fieldset')
form = tag('form', method='post')
h1 = tag('h1')
h2 = tag('h2')
h3 = tag('h3')
h4 = tag('h4')
h5 = tag('h5')
h6 = tag('h6')
head = tag("head")
hr = tag('hr/')
html = tag("html")
i = tag("i")
#img=tag('img/',border='0')
img = tag('img/')
input = tag('input/', type='text', value='')
label = tag('label')
legend = tag('legend')
li = tag('li')
link = tag('link/')
meta = tag('meta/')
noscript = tag('noscript')
ol = tag('ol')
option = tag('option')
p = tag("p")
pre = tag('pre')
script = tag('script', type="text/javascript")
select = tag('select')
small = tag("small")
span = tag("span")
table = tag('table')
tbody = tag('tbody')
td = tag('td')
textarea = tag('textarea', _clean=True)
tfoot = tag('tfoot')
th = tag('th')
thead = tag('thead')
title = tag("title")
tr = tag('tr')
ul = tag('ul')
nav = tag("nav")

# obsolete but useful
style = tag('style')
font = tag('font')
strong = tag('strong')

section = tag("section")
header = tag('header')
footer = tag('footer')
article = tag('article')

# Bootstrap
css = tag('link', rel="stylesheet", type="text/css")
row = tag('div', cls='row')
container = tag('div', cls="container")
fluid = tag('div', cls="container-fluid")
col1 = tag('div', cls="col-md-1")
col2 = tag('div', cls="col-md-2")
col3 = tag('div', cls="col-md-3")
col4 = tag('div', cls="col-md-4")
col5 = tag('div', cls="col-md-5")
col6 = tag('div', cls="col-md-6")
col7 = tag('div', cls="col-md-7")
col8 = tag('div', cls="col-md-8")
col9 = tag('div', cls="col-md-9")
col10 = tag('div', cls="col-md-10")
col11 = tag('div', cls="col-md-11")
col12 = tag('div', cls="col-md-12")
formgroup = tag('div', cls="form-group")

## logic


def let(namespace, **args):
    "allows local variable assignment, and macro blocks"
    namespace.update(args)
    return ""


def include(filename, ob, req):
    "call another template"
    return Evo(filename)(ob, req, wrapper=None)


def content(ob, req):
    "wrapper include"
    #  return Evo("").evaluate(ob._v_content_pyc,ob,req)
    #force to str to get rid of "WARNING: UNICODE STRING in evo/wrapper.evo - converted to str" errors in log
    return str(Evo("").evaluate(ob._v_content_pyc, ob, req))


namespace = locals()
