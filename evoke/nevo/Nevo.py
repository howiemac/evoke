"""
EvoParser
    Parse evo lines into Tokens

        Split a file into logical Evo lines.

        Parse each line into a token


Token
    A token is a callable node in an evo document

    A token has content - a list of contained tokens
                parse - a reference to the containing token
                indent - indent level in source code
                filename - source code, for debugging
                line - line number in source code

    Tokens have a __call__ method which renders them and
    their content.

    We will subclass token to create more specific functions

    TODO-FUTURE - build AST rather than token tree

"""
import re
import os
import ast
import pprint
import types
from copy import deepcopy
from munch import DefaultMunch
from io import StringIO
import gettext
import traceback
import html
from datetime import datetime



class Token(object):
    ""
    global_namespace = {}

    def __init__(self,
                 parent=None,
                 content=None,
                 indent=-1,
                 filename='',
                 line=0):
        ""
        self.parent = parent
        self.content = content or []
        self.indent = indent
        self.filename = filename
        self.line = line
        self.namespace = {}

    def get_namespace(self):
        """return own namespace plus that of any child ReferenceToken"""
        namespace = {}
        namespace.update(self.namespace)
        for token in [i for i in self.content if isinstance(i, ReferenceToken)]:
            namespace.update(token.get_namespace())
        return namespace

    def consume(self, token):
        """ Add a token to the content structure.
        Tokens with higher indentation are added to self.content
        If self.parent exists pass the token to parent
        otherwise raise an indentation error.

        Return the token.
    """
        if token.indent > self.indent:
            # all contained tokens must have the same indent
            content_indent = self.content_indent()
            if content_indent and content_indent != token.indent:
                raise EvoIndentationError(token, self.content[0].indent)
            token.parent = self
            if isinstance(token, CompoundToken):
                self.namespace[token.tagname] = token
                self.__class__.global_namespace[token.tagname] = token
            else:
                self.content.append(token)
                # IncludeTokens pass their namespace to their parent
                if isinstance(token, IncludeToken):
                    self.namespace.update(token.namespace)

                    # TODO: should we merge the namespaces?
                    token.namespace = self.namespace

            return token
        elif self.parent:
            return self.parent.consume(token)
            if isinstance(token, CompoundToken):
                self.parent.namespace[token.tagname] = token
        elif not self.content:
            # we are a new, empty root token.
            self.content.append(token)
            # IncludeTokens pass their namespace to their parent
            if isinstance(token, IncludeToken):
                self.namespace.update(token.namespace)
            if isinstance(token, CompoundToken):
                self.namespace[token.tagname] = token
            return token
        else:
            raise EvoIndentationError(token, self.indent)

    def content_indent(self):
        """return the indent of this token's content if it exists.
       There could be code objects included which have no indent
       of their own.

       All content should have identical indentation so return
       the first indent found.  Otherwise return None
    """
        for item in self.content:
            if hasattr(item, 'indent'):
                return item.indent
            # no indent found
            return None

    def __call__(self, env):
        "render this token and all its contained tokens"
        parts = [self.before(env)] + \
                [self.render_content(env)] + \
                [self.after(env)]

        # if there is an ExceptionContainer, return that
        for part in parts:
            if isinstance(part, ExceptionContainer):
                return part

        return ''.join(parts)

    def before(self, env):
        "data to show before this token's content"
        return ''

    def render_content(self, env):
        ""
        res = []
        env['__here__'] = self
        for item in self.content:
            if type(item) == types.CodeType:
                # evaluate and return as string
                res.append(str(eval(item, globals(), env)))
            else:
                # we have a token - call it
                content = self.maybe_content(item, env)
                if self.raised(content):
                    return content
                res.append(content)

        return ''.join(res)

    def after(self, env):
        "data to show after this token's content"
        return ''

    def next_sibling(self):
        """return next item after this token in parent.content"""
        content = self.parent.content
        index = content.index(self) + 1
        if index < len(content):
            return content[index]
        else:
            return None

    def describe(self, indent=0):
        """Describe template structure"""
        filename = self.filename.split('site-packages', 1)[-1]
        if self.__class__.__name__ == 'IncludeToken':
            s = "<div class='%s'>%s%s %s (%s) %d</div>" % (self.__class__.__name__, '&nbsp;' * (indent * 4), self.__class__.__name__, filename, self.included_filename, self.line or 0)
        elif self.__class__.__name__ == 'TagToken':
            s = "<div class='%s'>%s%s %s %s %d</div>" % (self.__class__.__name__, '&nbsp;' * (indent * 4), self.tagname, self.__class__.__name__, filename, self.line or 0)
        else:
            s = "<div class='%s'>%s%s %s %d</div>" % (self.__class__.__name__, '&nbsp;' * (indent * 4), self.__class__.__name__, filename, self.line or 0)
        if len(list(self.namespace.keys())):   
            ns = """<div class="namespace">NS %s</div>"""  % str(self.namespace.keys())
        else:
            ns = ''
        res = [s, ns]
        for i in self.content:
            if hasattr(i, 'describe'):
                res += i.describe(indent + 1)
            else:
                res.append(  ('&nbsp;' * (4*(indent + 1))) + html.escape(str(i)))
        return res        

    def parseAtts(self, source, filename=''):
        """parse content / attributes from a tag line
       into a dict of code objects / strings

       In the result '_' maps to the content parameter.
    """
        # empty source?
        if not source.strip():
            return {}
        # first we wrap our source in a notional function
        source = "fn(%s)" % source
        # then we parse it into an abstract syntax tree
        tree = ast.parse(source)
        # since this is a contrived setup we can go looking
        # for the parameters.
        call = tree.body[0].value
        # we assume there is only a single arg
        # TODO allow for multiple args which will be
        # concatenated at rendering time

        # this wasn't working because each call to
        # compile reuses the same slot so we end up with
        # lots of references to the same code object which
        # takes the value of the last compilation

        # This being the case we shall send raw code objects
        # and will evaluate them at run time.

        keywords = {}
        if call.args:
            content = call.args[0]
            expr = ast.Expression(body=content)
            code = compile(expr, filename, 'eval')
            keywords['_'] = code

        # keywords
        for i in call.keywords:
            # compile the value
            expr = ast.Expression(body=i.value)
            # wrap our code as a callable
            code = compile(expr, filename, 'eval')
            keywords[i.arg] = code

        return keywords

    def filenames(self):
        """return list of files contained within this token"""
        fileset = [self.filename]
        for i in self.content + list(self.namespace.values()):
            # include contained content
            # include compound tags
            if hasattr(i, 'filenames'):
                fileset += (i.filenames())
            elif hasattr(i, 'co_filename'):
                fileset += [i.co_filename]

        return fileset

    def last_update(self):
        """return unix time of most recently updated file included in this token"""
        updated = [os.path.getmtime(i) for i in self.filenames() if i.strip()]
        if updated:
            return max(updated)
        else:
            # no files involved
            return 0

    def maybe_content(self, fn, *a, **k):
        """Return the result of fn(*a, **k) or an ExceptionContainer
           taken is optional, used when fn is not Token.__call__
        """
        token = k.get('token', '')
        if token:
            del k['token']

        offset = k.get('offset', 0)
        if offset:
            del k['offset']

        try:
            content = fn(*a, **k)
        except Exception as e:
            content = ExceptionContainer(e, token or fn, a, k, offset)
        return content

    def raised(self, content):
        """"""
        return isinstance(content, ExceptionContainer)

    def context(self, offset=0):
        """Return source code with surrounding lines"""
        if not self.filename:
            return ''

        return _context(self.filename, self.line, offset)


def _context(filename, line, offset=0):
    """Render context of source file around line"""
    with open(filename) as f:
        l = list(f)
    line =  max(0, line - 1) + offset
    start = max(0, line-3)
    end = min(len(l), line + 3)
    _res = l[start:end]
    res = []
    for i in _res:
        if i == l[line]:
            res.append('<span style="color: red;">%s</span>' % i.replace(
                ' ', '&nbsp;'))
        else:
            res.append(i.replace(' ', '&nbsp;'))
    return '<pre>%s</pre>' % (''.join(res))


class ForToken(Token):
    """
    for <complex structure> in <iterator>:<content>
  """
    rx = re.compile('for (.*?) in (.*?)\:(.*?)$')

    @classmethod
    def make(self, data):
        """ If we have a for line, generate a ForToken.

        Extends token by adding

          self.varfn = compiled code to allocate iterator items to local variables
            eg.  for (x,(y,z)) in [...]: something
            we need a function of form   def varfn(x): ... ; return {vars}

          self.iterator = compiled code for the iterator to be cycled through.

        Expects data to be {indent:0, body:'', filename:'', line:0}
    """
        match = self.rx.match(data['body'])
        if not match:
            # not a ForToken
            return

        # create new ForToken
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        varexp, iterexp, contentexp = match.groups()

        # make sure there is content to the right of the colon
        if data['body'].strip().endswith(':'):
            data['body'] += ' "" '
        fnstr = """def fn():\n  %s""" % data['body']
        astm = ast.parse(fnstr, '', 'exec')
        forob = astm.body[0].body[0]
        token.iterator = compile(ast.Expression(forob.iter), filename, 'eval')
        if contentexp:
            token.content.append(
                compile(ast.Expression(forob.body[0].value), filename, 'eval'))

        # allocation function
        varfnstr = "def varfn(loopvalues): %s = loopvalues; return locals()" % varexp

        varfncode = compile(varfnstr, filename, 'exec')
        _globals = {}
        _locals = {}
        eval(varfncode, _globals, _locals)
        token.varfn = _locals[
            'varfn']  # a function which takes an iterator and returns a dict
        return token

    def __call__(self, env):
        """handle rendering
       render content for each iteration
       or if no content look for an else clause
       next in the parent's content list.  If it exists
       call its render_content method directly.
    """
        res = []
        # for each item in our iterator
        items = self.maybe_content(
            eval, self.iterator, globals(), env, token=self)
        if self.raised(items):
            return items

        for item in items:
            # extract parameters into a dict
            params = self.maybe_content(self.varfn, item, token=self)
            if self.raised(params):
                return params

            # apply parameters to a local copy of env
            localenv = deepcopy(env)
            #localenv = env
            localenv.update(params)

            offset = 0
            for item in self.content:
                offset += 1
                if type(item) == types.CodeType:
                    # evaluate and return as string
                    # TODO - this points to the for loop not the code line
                    content = self.maybe_content(
                        eval, item, globals(), localenv, offset=offset)
                    if self.raised(params):
                        return params

                    res.append(str(content))
                else:
                    # we have a token - call it
                    content = self.maybe_content(item, localenv)
                    if self.raised(content):
                        return content
                    res.append(content)
        else:
            # TODO - look for an else tag
            # is the next Token in the current context an IfToken?
            next_sibling = self.next_sibling()
            if isinstance(next_sibling, ElseFinallyToken):
                content = self.maybe_content(next_sibling.render_content, env)
                if self.raised(content):
                    return content
                res.append(content)

        return ''.join(res)


class IfToken(Token):
    """  Control token of form

       if <condition>:content

       Extends Token with self.condition

  """
    rx = re.compile('if (.*?):(.*?)$')
    tagname = 'if'

    @classmethod
    def make(self, data):
        """ If we have an if line, generate an IfToken.
    """
        match = self.rx.match(data['body'])
        if not match:
            # not a IfToken
            return

        # create new IfToken
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        # contrive a function, convert elif to if so it will parse
        fnstr = """def fn():\n  %s""" % (data['body'].replace('elif', 'if', 1))
        if fnstr.strip().endswith(':'):
            fnstr += ' ""'
            hasbody = False
        else:
            hasbody = True
        astm = ast.parse(fnstr, filename, 'exec')
        # test
        asttest = astm.body[0].body[0].test
        expr = ast.Expression(asttest)
        token.condition = compile(expr, filename, 'eval')
        # content
        if hasbody:
            astbody = astm.body[0].body[0].body[0].value
            expr = ast.Expression(astbody)
            token.content.append(compile(expr, filename, 'eval'))

        return token

    def __call__(self, env):
        """ handle rendering.

        If self.condition is true
          render self.content
        otherwise if our next_sibling
        is an elif or else render their content.
    """
        res = []

        condition = self.maybe_content(
            eval, self.condition, globals(), env, token=self)
        if self.raised(condition):
            return condition

        if condition:
            offset = 0
            for item in self.content:
                offset += 1
                if type(item) == types.CodeType:
                    # evaluate and return as string
                    content = self.maybe_content(
                        eval, item, globals(), localenv, offset=offset)
                    if self.raised(content):
                        return content

                    res.append(str(content))
                else:
                    # we have a token - call it
                    content = self.maybe_content(item, env)
                    if self.raised(content):
                        return content
                    res.append(content)
        else:
            # if our next_sibling is an Elif or an Else - add their content
            next_sibling = self.next_sibling()
            if isinstance(next_sibling, ElifToken):
                # try the next elif
                content = self.maybe_content(next_sibling, env, test=True)
                if self.raised(content):
                    return content
                res.append(content)
            elif isinstance(next_sibling, ElseFinallyToken):
                # show the content of the else
                content = self.maybe_content(next_sibling.render_content, env)
                if self.raised(content):
                    return content
                res.append(content)
            else:
                # nothing else to try
                pass

        return ''.join(res)


class ElifToken(IfToken):
    """ An Elif token parses identically to an if
      but is not triggered directly by its call
  """
    rx = re.compile('elif (.*?):(.*?)$')
    tagname = 'elif'

    def __call__(self, env, test=False):
        """when test is true we have been called by an
       IfToken so test our condition before rendering
       content.

       Otherwise return no content.
    """
        if test:
            return IfToken.__call__(self, env)
        else:
            return ''


class ElseFinallyToken(Token):
    """  Control tokens of form

       tagname: <content>

       These only trigger rendering when self.render is called, not when called directly.
  """

    rx = re.compile('(else|finally)\:(.*?)$')

    @classmethod
    def make(self, data):
        """ Generate an ElseFinally token if data['body'] matches.

        Extends Token by adding self.tagname = else | finally
    """
        match = self.rx.match(data['body'])
        if not match:
            # not an ElseFinally
            return

        # create new ElseFinally
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        token.tagname, contentexp = match.groups()
        contentexp = contentexp.strip()
        if contentexp:
            token.content.append(compile(contentexp, filename, 'eval'))

        return token

    def __call__(self, env):
        """ElseFinally tags produce no content when called directly"""
        return ''


#### try ... except* ... else ... finally ####
class TryToken(Token):
    """ try: <content>

      at render time attempt to render content
      but on error pass control to an adjacent
      except token.
  """
    rx = re.compile('try\:(.*?)$')
    tagname = 'try'

    @classmethod
    def make(self, data):
        match = self.rx.match(data['body'])
        if not match:
            # not an try
            return

        # create new TryToken
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        # Extract the content
        contentexp = match.groups()[0].strip()

        if contentexp:
            token.content.append(compile(contentexp, filename, 'eval'))

        return token

    def siblings(self):
        """return dict of except clauses, else and finally (if available)"""
        res = {'exc': [], 'else': None, 'finally': None}
        content = self.parent.content
        following = content[content.index(self) + 1:]
        while following:
            item = following.pop(0)
            if isinstance(item, ExceptToken):
                res['exc'].append(item)
            elif isinstance(item, ElseFinallyToken):
                res[item.tagname] = item
                if item.tagname == 'finally':
                    break
            else:
                # we have run out of useful siblings. Nothing more to add.
                break

        return res

    def __call__(self, env):
        """
      Handle rendering
      try: render content
      if successful look past any except siblings for an else or finally clause
      then pass rendering to that
      If an exception pass the exception raised to an adjacent except or finally clause.
      Ignore any else clauses beforehand.
    """
        res = []

        # find our available sibling group - excepts, maybe an else and maybe a finally
        siblings = self.siblings()

        try:
            for item in self.content:
                if type(item) == types.CodeType:
                    # evaluate and return as string
                    res.append(str(eval(item, globals(), env)))
                else:
                    # we have a token - call it, look out for partial rendering
                    try:
                        res.append(item(env))
                    except Exception as ETE:
                        # add any rendered content
                        if getattr(ETE, 'rendered_content', None):
                            res.append(ETE.rendered_content)
                            ETE.rendered_content = ''
                        # raise the original exception
                        raise

            # if we have made it to here without an error we can render
            # the else clause
            if siblings['else']:
                res.append(siblings['else'].render_content(env))
        except Exception as E:
            # search exception siblings for a matching exception.
            exc_type = E.__class__.__name__
            # go for the first matching type or the first with no type specified
            # or that is of type 'Exception'
            exception = None
            for exc in siblings['exc']:
                if exc.exc_type == exc_type or exc.exc_type in (None,
                                                                'Exception'):
                    exception = exc
                    break

            if exception:
                # if the exception has a name, drop this into a copy of env
                our_env = deepcopy(env)
                if exception.exc_name:
                    our_env[exception.exc_name] = E

                content = self.maybe_content(exception.render_content, our_env)
                if self.raised(content):
                    return content
                res.append(content)
            else:
                # The exception remains uncaught
                # trigger finally
                if siblings['finally']:
                    content = self.maybe_content(
                        siblings['finally'].render_content, env)
                    if self.raised(content):
                        return content
                    res.append(content)

                # send any content already rendered back up the tree
                E.rendered_content = ''.join(res)
                raise

        # always render the finally clause
        if siblings['finally']:
            content = self.maybe_content(siblings['finally'].render_content,
                                         env)
            if self.raised(content):
                return content

            res.append(content)

        return ''.join(res)


class ExceptToken(Token):
    """Except variations"""
    rx = re.compile('except(( )?(.*?)( as (.*?))?):(.*?)$')
    tagname = 'except'

    @classmethod
    def make(self, data):
        match = self.rx.match(data['body'])
        if not match:
            # not an ElseFinally
            return

        _, _, exception, _, exception_name, content = match.groups()

        # create new ExceptToken
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        # Though of marginal benefit fetch content from the ast.
        # Make sure there is content
        if data['body'].strip().endswith(':'):
            data['body'] += ' pass'
            has_body = False
        else:
            has_body = True

        fnstr = """def fn():
  try:
    ""
  %s
      """ % data['body']
        t = ast.parse(fnstr, filename, 'exec')
        handler = t.body[0].body[0].handlers[0]
        token.exc_name = handler.name and handler.name or None
        token.exc_type = handler.type and handler.type.id or None
        if has_body:
            token.content.append(
                compile(
                    ast.Expression(handler.body[0].value), filename, 'eval'))

        return token

    def __call__(self, env):
        """Exception tags produce no content when called directly"""
        return ''


class TagToken(Token):
    """ General tags

      tagname: <content>?, <k>=<v>*

      adds:
        self.tagname
        self.atts

  """
    rx = re.compile('(?P<tag>\w+)\:(?P<atts>.*)')

    # tags which can be void in html 5
    void_tags = (
            'area',
            'base',
            'br',
            'col',
            'command',
            'embed',
            'hr',
            'img',
            'input',
            'keygen',
            'link',
            'meta',
            'head',
            'param',
            'source',
            'track',
            'wbr',
            )

    # atts that use their own name as a value in xhtml
    single_atts = ['selected', 'checked', 'disabled']

    @classmethod
    def make(self, data):
        """ Generate a TagToken if data['body'] matches.

    """
        match = self.rx.match(data['body'])
        if not match:
            # not a tag
            return

        # create new tag
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        # Parse tag
        token.tagname, atts = self.rx.match(data['body']).groups()
        atts = self.safeKeywords(atts)
        atts = token.parseAtts(atts, filename)
        if '_' in atts:
            token.content.append(atts['_'])
            del atts['_']
        token.atts = self.restoreKeywords(atts)

        return token

    #### Hide Python keywords from parser
    keyword_replacements = [('class', 'cls'), ('for', 'for_id'), ('-', '_')]

    @classmethod
    def safeKeywords(self, atts, rx=re.compile('([A-Za-z0-9-]+\W*=)')):
        """make keywords eg. class, data-* safe for parsing.
       Expects atts to be a string of key=value
    """

        def replace(m):
            src = m.groups()[0]
            for fr, to in self.keyword_replacements:
                src = src.replace(fr, to)
            return src

        return rx.sub(replace, atts)

    @classmethod
    def restoreKeywords(self, atts):
        "restore names of keywords replaced by safeKewords"
        res = {}
        for k, v in atts.items():
            for to, fr in self.keyword_replacements:
                k = k.replace(fr, to)
            res[k] = v
        return res

    #### Render ####
    def __call__(self, env):
        """
    look for a compound token in the parent namespace
    and pass rendering to it, otherwise render as normal
    """
        in_here = False
        in_wrapper = False
        parent = self.parent
        while parent:
            parent_namespace = parent.get_namespace()
            if self.tagname in parent_namespace:
                return parent_namespace[self.tagname](env, content=self)
            else:
                parent = parent.parent
                if (not parent) and (not in_wrapper) and '__wrapper__' in env:
                    # move into the wrapper, if it is there
                    parent = env['__wrapper__']
                    in_wrapper = True

        # Finally try the global namespace
        if self.tagname in self.__class__.global_namespace:
            return self.__class__.global_namespace[self.tagname](env, content=self)

        # not a compound tag
        return Token.__call__(self, env)

    def before(self, env):
        """Render the start tag, or the entire tag if no content"""
        atts = self.renderAtts(env)
        tail = self.tagname in self.void_tags and (not self.content) and '/' or ''
        tag_content = ' '.join(i for i in (atts, tail) if i)
        return '<%s%s>' % (self.tagname, tag_content)

    def renderAtts(self, env):
        "render atts in alphabetical order"
        if self.atts:
            res = []
            for k, v in sorted(self.atts.items()):
                v = eval(v, globals(), env)
                # handle single atts - use their names as a value
                # or omit them entirely
                if k in self.single_atts:
                    if v:
                        v = k
                    else:
                        continue
                res.append('%s="%s"' % (k, v))
            return ' %s' % (' '.join(res))
        else:
            return ''

    def after(self, env):
        """Render an end tag if needed"""
        if self.content or self.tagname not in self.void_tags:
            return '</%s>' % self.tagname
        else:
            return ''


class TagNoSingletonToken(TagToken):
    """For tags which cannot be singletons.  So far only script:"""
    rx = re.compile('(?P<tag>script|i)\:(?P<atts>.*)')

    def before(self, env):
        """Render the start tag"""
        atts = self.renderAtts(env)
        return '<%s%s>' % (self.tagname, atts)

    def after(self, env):
        """Render an end tag.  The whitespace is needed."""
        return ' </%s>' % self.tagname


class CodeToken(Token):
    """ A code token compiles a plain Python expression
      and stores it in self.content
  """

    @classmethod
    def make(self, data):
        """compile a code expression. Store it in self.content"""

        if not data['body'].strip():
            return

        # create new CodeToken
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        try:
            token.content = [compile(data['body'], filename, 'eval')]
        except SyntaxError:
            # may be an exec statement - try that
            try:
                token.content = [compile(data['body'], filename, 'exec')]
            except:
                raise EvoSyntaxError(token, data)
        except:
            raise EvoSyntaxError(token, data)

        return token

    def __call__(self, env):
        """evaluate our single content item. Return it as string"""
        if self.content:
            try:
                #res = str(eval(self.content[0], globals(), env) or '')
                # it seems that list comprehensions depend on env being in the globals
                globals().update(env)
                res = str(eval(self.content[0], globals(), env) or '')
                if type(res) == str:
                    return res
                else:
                    return str(res)
            except:
                raise EvoRuntimeError(self)
        else:
            return ''


class IncludeToken(Token):
    """ To include a separate Evo file
      we parse it into its own token
      then add a context which will
      be passed as self at render time.
  """
    rx = re.compile('(.*\.)?([^\.]+.evo)$')

    @classmethod
    def make(self, data, parser):
        "compile an include"
        match = self.rx.match(data['body'])
        if not match:
            # not an Include
            return

        context_text, included_filename = match.groups()
        #self.included_filename = included_filename


        # make sure our own path is available to the parser
        path, _ = os.path.split(data['filename'])

        original_paths = parser.paths[:]

        if path in original_paths:
            parser_paths = original_paths[:]
        else:    
            parser_paths = [path] + original_paths[:]
        parser.paths = original_paths[:]

        # Parse the inclusion into the current token
        token = parser.parse(paths=parser_paths, filename=included_filename, root=self())

        # reinstate original paths
        parser.paths = original_paths

        token.filename = data['filename']
        token.included_filename = included_filename
        token.indent = data['indent']
        token.line = data['line']

        # define context if available
        if context_text:
            # strip off the trailing '.'
            token.context = compile(context_text[:-1], data['filename'], 'eval')
        else:
            token.context = None

        return token

    def __call__(self, env):
        """set the context if it exists"""
        our_env = deepcopy(env)
        #our_env = env
        if self.context:
            our_env['self'] = eval(self.context, globals(), our_env)
        return Token.__call__(self, our_env)


class ReferenceToken(Token):
    """ A lazy reference to an IncludeToken. Nb. not a full Token """
    def __init__(self,
                 parent=None,
                 content=None,
                 indent=-1,
                 filename='',
                 line=0,
                 cache=None,
                 cache_id=None
                 ):
        Token.__init__(self, parent, content, indent, filename, line)
        self.cache = cache
        self.cache_id = cache_id

    @classmethod
    def make(self, data, cache, cache_id):
        "compile an include"
        token = self(filename=data['filename'], indent=data['indent'], line=data['line'], cache=cache, cache_id=cache_id)
        return token
    
    def __call__(self, env):
        """pass call to referenced token"""
        token = self.cache[self.cache_id]
        return token.__call__(env)
        
    def get_namespace(self):
        """return namespace of IncludeToken"""
        return self.cache[self.cache_id].namespace

    def filenames(self):
        """pass call to IncludeToken"""
        return self.cache[self.cache_id].filenames()


class CompoundToken(TagToken):
    """ defines compound tokens eg.

      def fancybutton: __content__, class="defaultclass", data-thing="default-thing"
        div: class="defaultbuttonclass " + class
          button: data-thing=data-thing
            contentgoeshere

      called as:

      fancybutton: "a button", class="fancyclass"

      which renders as

      div: class="defaultbuttonclass fancyclass"
        button: data-thing="default-thing"
          "a button"

      Until we decide otherwise these will have their own namespace and will
      be substituted at compilation time in place of standard TagTokens
  """

    class Identity(dict):
        """any key will return itself as a value"""

        def __getitem__(self, k):
            return k

    identity = Identity()

    rx = re.compile('def (?P<tag>\w+)\:(?P<atts>.*)')

    @classmethod
    def make(self, data):
        """ Generate a CompoundToken if data['body'] matches.

    """
        match = self.rx.match(data['body'])
        if not match:
            # not a compound tag
            return

        # create new tag
        filename = data['filename']
        token = self()
        token.indent = data['indent']
        token.filename = filename
        token.line = data['line']

        # Parse tag
        token.tagname, atts = self.rx.match(data['body']).groups()
        atts = self.safeKeywords(atts)
        atts = token.parseAtts(atts, filename)
        if '_' in atts:
            # inelegant hack to extract placeholder name from code object
            ph = eval(atts['_'], {}, self.identity)
            token.placeholder = ph
            del atts['_']
        else:
            token.placeholder = '__content__'
        token.atts = self.restoreKeywords(atts)
        return token

    def before(self, env):
        ""
        return ''

    def after(self, env):
        ""
        return ''

    def __call__(self, env, content=None):
        ""
        # override defaults
        _ourenv = deepcopy(self.atts)
        if content:
            _ourenv.update(deepcopy(content.atts))
        ourenv = {}
        for k, v in _ourenv.items():
            ourenv[k] = eval(v, globals(), env)
        ourenv.update(deepcopy(env))
        # embed content
        if content:
            content = self.maybe_content(
                content.render_content, ourenv, token=content)
            if self.raised(content):
                return content
        else:
            content = ''
        ourenv[self.placeholder] = content
        return Token.__call__(self, ourenv)


class EvoParser(object):
    "Parse text into a tree of Tokens"
    token_precedent = [
        IncludeToken, ForToken, IfToken, ElifToken, TryToken, ExceptToken,
        ElseFinallyToken, CompoundToken, TagNoSingletonToken, TagToken,
        CodeToken
    ]

    includeTokens = {}   # cache of IncludeTokens

    def __init__(self, paths=['.']):
        """set up paths to look for evo files"""
        self.paths = paths

    def parse(self, paths=None, file=None, filename='', content=None, root=None):
        """ Parse content into a tree of tokens.
    """

        log = open('/tmp/nevo.txt', 'a')

        lines = self.line_generator(
            paths=paths, file=file, filename=filename, content=content)
        root = (root or Token())
        here = root
        log.write('%s\n' % filename)
        for line in lines:
            here.parser = self
            token = self.make_token(line)
#            try:
#                token = self.make_token(line)
#            except Exception as e:
#                return ParseExceptionContainer(e, filename, line['line'])
#            if isinstance(token, ParseExceptionContainer):
#                return token
            here = here.consume(token)
        return root

    def make_token(self, line):
        """ Identify and generate token.
        Expects dict as output by self.line_generator
    """
        # return the first token that recognises the line
        for T in self.token_precedent:
            if T == IncludeToken:
                # pass this parser on to Include Token
                #token = T.make(line, self)
                token = self.make_include_token(line)
            else:
                token = T.make(line)
            if token:
                return token
            # no token - raise an EvoSyntaxError
        raise EvoSyntaxLineError(line)

    def make_include_token(self, line):
        """ make an IncludeToken or a reference token
            if the IncludeToken already exists.
        """
        # check if line matches regular expression.  Bail if it doesn't
        if not IncludeToken.rx.match(line['body']):
            return

        cache_id = line['body']

        # if there is already a reference in self.includeTokens
        # return a ReferenceToken
        if cache_id in self.includeTokens:
            token = ReferenceToken.make(line, self.includeTokens, cache_id)
            return token

        # Create a default reference in cache
        self.includeTokens.setdefault(cache_id, None)
        token = IncludeToken.make(line, self)
        self.includeTokens[cache_id] = token
        return token

    

    def find_file(self, filename, paths=None):
        """look for filename in self.paths"""
        paths = paths or self.paths
        for path in paths:
            fname = os.path.join(path, filename)
            if os.path.exists(fname):
                return fname
        # None found
        raise EvoPathError(filename, self.paths)

    def line_generator(self,
                       paths=None,
                       file=None,
                       filename=None,
                       content=None,
                       rx=re.compile('(\s*)(.*)')):
        """ generate logical Evo line.

        Return {text, filename, line}

        TODO: make this more sophisticated

        Content can be a filename, an open file or a string.

    """
        if file:
            pass
        elif filename:
            filename = self.find_file(filename, paths=paths)
            file = open(filename)
        elif content:
            file = StringIO(content)
        else:
            # nothing here
            file = StringIO()

        line = 0
        _line = 0
        for text in file:
            line += 1
            match = rx.match(text)
            if match:
                indent, body = match.groups()
                # omit comment lines
                if body.strip() and not body.startswith('#'):
                    _line += 1
                    yield {
                        'indent': len(indent),
                        'body': body.rstrip(),
                        'filename': filename,
                        'line': line
                    }
        file.close()


class NevoTemplate(object):
    """
  Manage content and wrapper templates - update each
  if necessary.
  """

    def __init__(self, filename, wrapper='wrapper.evo', paths=['.', 'evo']):
        """ """
        # allow for multiple wrappers
        self.filename = filename
        self.wrappername = wrapper
        self.parser = EvoParser(paths=paths)
        self.cache = {}  # {filename: {token, last_update}}
        self.template = self.fetch(filename)
        self.wrapper = self.fetch(wrapper)
        self.paths = paths

    def fetch(self, filename):
        """fetch token associated with filename, recompiling
       if newer version of the file is available
    """
        # have we a valid cached version?
        if filename in self.cache:
            x = self.cache[filename]
            token = x['token']
            last_update = x['last_update']
            new_last_update = token.last_update()
            if last_update >= new_last_update:
                # the token is up to date
                print('token up to date')
                return token

        # re-compile the token
        return self.recompile(filename)

    def recompile(self, filename):
        token = self.parser.parse(filename=filename)
        last_update = token.last_update()
        self.cache[filename] = {'token': token, 'last_update': last_update}
        return token

    def __call__(self, env):
        """render the template - allow for wrapper override"""
        # fetch the wrapper
        if 'wrapper' in env:
            if env['wrapper']:
                wrapper = self.fetch(env['wrapper'])
                if isinstance(wrapper, ParseExceptionError):
                    return str(wrapper)
            else:
                # no wrapper
                wrapper = None

        # fetch the content template
        template = self.fetch(self.filename)

        # render
        if self.wrapper:
            # include the template as the __content__ function
            def __content__():
                # we set wrapper so TagTokens can search for
                # compound tags in the wrapper
                env['__wrapper__'] = env['__here__']
                return template(env)

            env['__content__'] = __content__
            wrapper = self.wrapper
        else:
            # use the template on its own
            wrapper = template

        return wrapper(env)

    def describe(self, env):
        """describe the token hierarchy"""
        # fetch the wrapper
        if 'wrapper' in env:
            if env['wrapper']:
                wrapper = self.fetch(env['wrapper'])
                if isinstance(wrapper, ParseExceptionError):
                    return str(wrapper) 
            else:
                # no wrapper
                wrapper = None

        # fetch the content template
        template = self.fetch(self.filename)

        # render
        if self.wrapper:
            # include the template as the __content__ function
            def __content__():
                return template.describe()

            env['__content__'] = __content__
            wrapper = self.wrapper
        else:
            # use the template on its own
            wrapper = template

        if wrapper == template:
            return wrapper.describe()
        else:
            return wrapper.describe() + ['<hr />'] + template.describe()

#### TODO: this should be put into Evoke as it is Evoke specific
try:
    from evoke import lib
except ImportError:
    try:
        # legacy support
        from base import lib
    except ImportError:
        lib = None


class NevoDecorator(object):
    """Wrap a method using a Nevo template"""

    @classmethod
    def make(self, classname, bases=['.']):
        """Generate a likely set of paths relative to classname

           Expects this directory pattern (precedence)

           evoke
               evo              (4)
               <classname>
                   evo          (2)
               <appname>
                   evo          (3)
                   <classname>
                       evo      (1)

        """
        paths = []
        for base in bases:
            paths.append(os.path.abspath('%s/%s/evo' % (base, classname)))
            paths.append(os.path.abspath('%s/evo' % (base, )))

#        paths = [
#            os.path.abspath(i)
#            for i in [
#                os.path.abspath(here+'/'+i) for i in [
#                    "./%s/evo" % classname, 
#                    "./evo",
#                    "../../evoke/%s/evo" % classname, 
#                    "../../evoke/evo"
#                    "../../../site-packages/evoke_git/evoke/%s/evo" % classname, 
#                    "../../site-packages/evoke_git/evoke/evo"
#                            ]
#                        ]
#                    ]
        return self(paths)

    def __init__(self, paths=['.', 'evo'], *a, **k):
        ""
        self.paths = paths
        self.a = a
        self.k = k

    def __call__(self, fn):
        "find the correct name of the template for this function then call it.  Allow for classmethods and kind override"
        fname = fn.__name__

        # we don't know much about this method at class creation time eg. it won't become a method
        # or classmethod till after this decorator is called.
        self.template_cache = {}

        def function(inner_self, req, *a, **k):
            "a typical template"

            # we know more about the method when it is called.
            # is this a classmethod?
            if type(inner_self) == type:
                klass = inner_self
            else:
                klass = type(inner_self)

            # Evo objects have low __init__ cost so we can keep the hierarchy in memory
            klass_names = []
            if hasattr(inner_self, '__override_classname__'):
                klass_names.append(inner_self.__override_classname__)
            klass_names += [i.__name__ for i in klass.__bases__]

            # set req._v_template_name to allow us to access the name of the main page template from within the template or handler
            req._v_template_name = fname
            # set language
            lang = self.getLang(inner_self, req)
            req.gettext = lang
            # run the function, and return the template
            # TODO: we should do something with the return value of fn.
            fn(inner_self, req)
            # try each template in turn
            for klass_name in klass_names:
                template_name = '%s_%s.evo' % (klass_name, fname)
                try:
                    if template_name in self.template_cache:
                        template = self.template_cache[template_name]
                    else:
                        template = NevoTemplate(template_name, paths=self.paths)
                        self.template_cache[template_name] = template
                    env = {'self': inner_self, 'req': req, 'gettext': lang, 'lib': lib}
                    if hasattr(req, 'wrapper'):
                        env['wrapper'] = req.wrapper

                    # make the token tree available for inspection
                    if '__describe__' in req:
                        style="""
                        <style>
                            .describe { font-family: courier; background-color: black; color: white; }
                            .IfToken, .ElseToken, .ElseFinallyToken, .ForToken { color: yellow; }
                            .IncludeToken { color: purple; font-weight: bold; }
                            .TagToken, .TagNoSingletonToken { color: cyan; }
                            .CodeToken { color: pink; }
                        </style>
                        """
                        return "%s<div class='describe'>%s</div>" % (style, '\n'.join(template.describe(env)))
                    else:
                        return template(env)
                except OSError:
                    pass
                except EvoPathError:
                    pass
            # no template found in the hierarchy
            raise FileNotFoundError([
                '%s_%s.evo' % (klass_name, fname) for klass_name in klass_names
            ])

        return function

    def getLang(self, inner_self, req):
        "return gettext language object"
        code = req.cookies.get('lang', '')
        if code:
            try:
                trans_domain = inner_self.Config.appname
                trans_path = os.path.join(inner_self.Config.app_fullpath,
                                          'trans')
                lang = gettext.translation(
                    trans_domain, trans_path, languages=[code])

            except:
                # fallback on plain, no-op gettext
                lang = gettext
            return lang.gettext
        else:
            #      print "no language specified"
            #return gettext.gettext
            return gettext


nevo = NevoDecorator()


class EvoIndentationError(Exception):
    "A token was passed with as invalid indentation"

    def __init__(self, token, expected_indent='unknown'):
        ""
        self.token = token
        self.expected_indent = expected_indent

    def __str__(self):
        ""
        return "Indentation error in %s line %d. Indent was %d.  Expected %s" % (
            self.token.filename, self.token.line, self.token.indent,
            str(self.expected_indent))


class EvoSyntaxError(Exception):
    "A token has encountered a syntax error"

    def __init__(self, token, data=None):
        ""
        self.token = token
        self.data = data

    def __str__(self):
        ""
        if self.data:
            data = str(self.data)
        else:
            data = ''
        return "Evo Syntax in %s line %d %s" % (self.token.filename,
                                                self.token.line, data)


class EvoSyntaxLineError(Exception):
    """A line has not been recognised as a token
     Expects a dict as yielded by Parser.line_generator
  """

    def __init__(self, line):
        ""
        self.line = line

    def __str__(self):
        ""
        return '\n'.join(
            ["Evo line not recognised.",
             pprint.pformat(self.line)])


class EvoPathError(Exception):
    """ The evo file has not been found at any available path.
  """

    def __init__(self, filename, paths):
        ""
        self.filename = filename
        self.paths = paths

    def __str__(self):
        ""
        return '%s not found in %s' % (self.filename, str(self.paths))


class EvoRuntimeError(Exception):
    """Error raised at runtime"""

    def __init__(self, token):
        """"""
        self.token = token

    def __str__(self):
        """"""
        return "EvoRuntimeError: %s line %d %s" % (self.token.filename,
                                                   self.token.line,
                                                   self.token.content[0])


class ExceptionContainer(object):
    """Contains an exception to be passed up the rendering hierarchy"""

    def __init__(self, exc, fn=None, a=[], k={}, offset=0):
        self.exc = exc
        self.fn = fn
        self.a = a
        self.k = k
        self.offset = offset

    def __str__(self):
        """"""
        parts = ['<h1>Error</h1>']
        try:
            raise self.exc
        except:
            if isinstance(self.fn, Token):
                parts.append('%s line %d' % (self.fn.filename, self.fn.line))
                try:
                    parts.append(self.fn.context(offset=self.offset))    
                except:    
                    parts.append(str(self.fn.context))    
            else:
                parts.append(
                    html.escape('args=%s kw=%s' % (str(self.a), str(self.k))))
            parts.append(
                html.escape(traceback.format_exc()).replace('\n', '<br />'))
        return '\n<br />\n'.join(parts)


class ParseExceptionContainer(object):
    """Contains an exception found at parse time.  Can be called as a token."""

    def __init__(self, exc, filename, line):
        """"""
        self.exc = exc
        self.filename = filename
        self.line = line

    def __call__(self, *a, **k):
        return self.__str__()

    def __str__(self):
        parts = ['<h1>Parse Error</h1>']
        try:
            raise self.exc
        except EvoPathError:
            parts.append('%s line %d' % (self.filename, self.line))
            parts.append('Not found: %s' % self.exc.filename)
        except:
            parts.append('%s line %d' % (self.filename, self.line))
            try:
                parts.append(_context(self.filename, self.line))
            except FileNotFoundError:
                pass
            parts.append(
                html.escape(traceback.format_exc()).replace('\n', '<br />'))
        return '\n<br />\n'.join(parts)

    def last_update(self):
        return datetime.now()
