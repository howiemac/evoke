import unittest
from Nevo import Token, IncludeToken, ForToken, IfToken, ElifToken, ElseFinallyToken, TagToken, CodeToken, EvoIndentationError, EvoSyntaxError, EvoPathError, TryToken, ExceptToken, CompoundToken, NevoTemplate
from Nevo import EvoParser
from io import StringIO


class TokenTestCase(unittest.TestCase):
    ""

    def testCreateToken(self):
        ""
        token = Token()
        self.assertFalse(token.parent)
        self.assertEqual(token.content, [])
        self.assertEqual(token.indent, -1)
        self.assertEqual(token.filename, '')
        self.assertEqual(token.line, 0)

    def testConsumeHigherIndent3Levels(self):
        "The inner tokens will be in the outer token's content"
        here = outer_token = Token()
        for i in range(3):
            token = Token(indent=i * 2)
            here = here.consume(token)
        self.assertEqual(here.parent.parent.parent, outer_token)

    def testConsumeCorrectDedentOneLevel(self):
        "add a correctly dedented token"
        here = outer_token = Token()
        for i in range(3):
            token = Token(indent=i * 2)
            here = here.consume(token)
        # indent -1 0 2 4
        token = Token(indent=2)
        here = here.consume(token)
        self.assertIn(token, outer_token.content[0].content)

    def testConsumeCorrectDedentToZero(self):
        "add a correctly dedented token"
        here = outer_token = Token()
        for i in range(3):
            token = Token(indent=i * 2)
            here = here.consume(token)
        # indent -1 0 2 4
        token = Token(indent=0)
        here = here.consume(token)
        self.assertIn(token, outer_token.content)

    def testConsumeBadDedent(self):
        "try to add a token with an invalid indent."
        here = outer_token = Token()
        for i in range(3):
            token = Token(indent=i * 2)
            here = here.consume(token)
        # indent -1 0 2 4
        token = Token(indent=1)
        self.assertRaises(EvoIndentationError, here.consume, token)

    def testConsumeCall(self):
        "render a token structure.  Expect an empty string"
        here = outer_token = Token()
        for i in range(3):
            token = Token(indent=i * 2)
            here = here.consume(token)
        self.assertEqual(outer_token({}), '')

    def testNextSiblingNoOther(self):
        """when there are no siblings, 
       expect next_sibling to return None"""
        here = outer_token = Token()
        token = Token(indent=2)
        here = here.consume(token)
        self.assertFalse(token.next_sibling())

    def testNextSibling(self):
        """for the last sibling   
       expect next_sibling to return None"""
        here = outer_token = Token()
        token1 = Token(indent=2)
        token2 = Token(indent=2)
        token3 = Token(indent=2)
        here = here.consume(token1)
        here = here.consume(token2)
        here = here.consume(token3)
        # first returns second; second returns third
        self.assertEqual(token1.next_sibling(), token2)
        self.assertEqual(token2.next_sibling(), token3)
        # last in the queue
        self.assertFalse(token3.next_sibling())


class ForTokenTestCase(unittest.TestCase):
    """ ForToken tests
  """

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {'indent': 0, 'body': '', 'filename': '', 'line': 0}
        res = ForToken.make(data_in)
        self.assertFalse(res)

    def testMakeMatchSingleVar(self):
        """match - single variable"""
        data_in = {
            'indent': 0,
            'body': 'for x in range(1): x',
            'filename': '',
            'line': 0
        }
        token = ForToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}

        # have we an iterator?
        itervalue = eval(token.iterator, _globals, _locals)
        self.assertEqual(itervalue, range(0, 1))

        # have we a varexp?
        params = token.varfn(itervalue[0])
        self.assertEqual(params['x'], 0)

        # can we apply our params to the content
        content = eval(token.content[0], _globals, params)
        self.assertEqual(content, 0)

    def testMakeMatchMultiVar(self):
        """simple list of parameters"""
        data_in = {
            'indent': 0,
            'body': 'for x,y in [(1,2), (3,4)]: x+y',
            'filename': '',
            'line': 0
        }
        token = ForToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}

        # have we an iterator?
        itervalue = eval(token.iterator, _globals, _locals)
        self.assertEqual(itervalue, [(1, 2), (3, 4)])

        # have we a varexp?
        params = token.varfn(itervalue[0])
        self.assertEqual(params['x'], 1)
        self.assertEqual(params['y'], 2)

        # can we apply our params to the content
        content = eval(token.content[0], _globals, params)
        self.assertEqual(content, 3)

    def testMakeMatchComplexVar(self):
        """simple list of parameters"""
        data_in = {
            'indent': 0,
            'body': 'for (x,(y,z)) in [(1,(2,3)), (3,(4,5))]: x+y+z',
            'filename': '',
            'line': 0
        }
        token = ForToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}

        # have we an iterator?
        itervalue = eval(token.iterator, _globals, _locals)
        self.assertEqual(itervalue, [(1, (2, 3)), (3, (4, 5))])

        # have we a varexp?
        params = token.varfn(itervalue[0])
        self.assertEqual(params['x'], 1)
        self.assertEqual(params['y'], 2)
        self.assertEqual(params['z'], 3)

        # can we apply our params to the content
        content = eval(token.content[0], _globals, params)
        self.assertEqual(content, 6)


class IfTokenTestCase(unittest.TestCase):
    """ IfToken tests
  """

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {
            'indent': 0,
            'body': 'notif: blah',
            'filename': '',
            'line': 0
        }
        res = IfToken.make(data_in)
        self.assertFalse(res)

    def testMakeMatchSimpleConditionNoContent(self):
        """match simple condition - no content"""
        data_in = {'indent': 0, 'body': 'if 1:', 'filename': '', 'line': 0}
        token = IfToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}
        self.assertTrue(eval(token.condition, _globals, _locals))
        self.assertFalse(token.content)

    def testMakeMatchSimpleConditionContent(self):
        """match simple condition - has content"""
        data_in = {
            'indent': 0,
            'body': 'if 1:"True"',
            'filename': '',
            'line': 0
        }
        token = IfToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}
        self.assertTrue(token.content)
        self.assertTrue(eval(token.condition, _globals, _locals))

    def testMakeMatchSimpleConditionContent2(self):
        """match simple condition - has content"""
        data_in = {'indent': 0, 'body': 'if True:2', 'filename': '', 'line': 0}
        token = IfToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}
        self.assertTrue(token.content)
        self.assertTrue(eval(token.condition, _globals, _locals))

    def testMakeMatchSimpleConditionContent3(self):
        """match simple condition - has content"""
        data_in = {
            'body': 'if True: 2',
            'filename': 'evo/AllTokenTypes.evo',
            'indent': 0,
            'line': 3
        }
        token = IfToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}
        self.assertTrue(token.content)
        self.assertTrue(eval(token.condition, _globals, _locals))

    def testMakeMatchConditionWithSlice(self):
        """match condition containing : 
       This is something larger that needs to be tackled
       separately.  For now fancy slices should have their
       own lines.
    """


#    data_in = {'indent': 0, 'body':'if range(10)[:2]:"True"', 'filename':'', 'line':0}
#    token = IfToken.make(data_in)
#    self.assertTrue(token)
#    _globals = {}
#    _locals = {}
#    self.assertTrue(token.content)
#    self.assertTrue(eval(token.condition,_globals, _locals))


class ElifTokenTestCase(unittest.TestCase):
    """ ElifToken tests
  """

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {'indent': 0, 'body': 'if: blah', 'filename': '', 'line': 0}
        res = ElifToken.make(data_in)
        self.assertFalse(res)

    def testMakeMatchSimpleConditionNoContent(self):
        """match simple condition - no content"""
        data_in = {'indent': 0, 'body': 'elif 1:', 'filename': '', 'line': 0}
        token = ElifToken.make(data_in)
        self.assertTrue(token)
        _globals = {}
        _locals = {}
        self.assertTrue(eval(token.condition, _globals, _locals))
        self.assertFalse(token.content)


class ElseFinallyTokenTestCase(unittest.TestCase):
    """ ForToken tests
  """

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {
            'indent': 0,
            'body': 'notelse: blah',
            'filename': '',
            'line': 0
        }
        res = ElseFinallyToken.make(data_in)
        self.assertFalse(res)

    def testMakeMatchElse(self):
        """match else:"""
        data_in = {'indent': 0, 'body': 'else:', 'filename': '', 'line': 0}
        token = ElseFinallyToken.make(data_in)
        self.assertTrue(token)
        self.assertEqual(token.tagname, 'else')

    def testMakeMatchFinally(self):
        """match finally:"""
        data_in = {'indent': 0, 'body': 'finally:', 'filename': '', 'line': 0}
        token = ElseFinallyToken.make(data_in)
        self.assertTrue(token)
        self.assertEqual(token.tagname, 'finally')

    def testMakeMatchElseWithContent(self):
        """match else:"""
        data_in = {
            'indent': 0,
            'body': 'else: "else"',
            'filename': '',
            'line': 0
        }
        token = ElseFinallyToken.make(data_in)
        self.assertTrue(token)
        self.assertEqual(token.tagname, 'else')
        self.assertTrue(token.content)


class TryTokenTestCase(unittest.TestCase):
    ""

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {'indent': 0, 'body': 'notatry', 'filename': '', 'line': 0}
        res = TryToken.make(data_in)
        self.assertFalse(res)

    def testTryNoContent(self):
        """a try with no content"""
        data_in = {'indent': 0, 'body': 'try:', 'filename': '', 'line': 0}
        res = TryToken.make(data_in)
        self.assertTrue(res)
        self.assertFalse(res.content)

    def testTryContent(self):
        """a try with no content"""
        data_in = {'indent': 0, 'body': 'try:"yes"', 'filename': '', 'line': 0}
        res = TryToken.make(data_in)
        self.assertTrue(res)
        self.assertTrue(res.content)


class ExceptTokenTestCase(unittest.TestCase):
    ""

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {
            'indent': 0,
            'body': 'notanexcept',
            'filename': '',
            'line': 0
        }
        res = ExceptToken.make(data_in)
        self.assertFalse(res)

    def testMakeMatchBare(self):
        """matching bare except"""
        data_in = {'indent': 0, 'body': 'except:', 'filename': '', 'line': 0}
        res = ExceptToken.make(data_in)
        self.assertTrue(res)
        self.assertFalse(res.exc_name)
        self.assertFalse(res.exc_type)
        self.assertFalse(res.content)

    def testMakeMatchExceptionNoName(self):
        """except with unnamed but specified exception type"""
        data_in = {
            'indent': 0,
            'body': 'except ValueError:',
            'filename': '',
            'line': 0
        }
        res = ExceptToken.make(data_in)
        self.assertTrue(res)
        self.assertFalse(res.exc_name)
        self.assertEqual(res.exc_type, 'ValueError')
        self.assertFalse(res.content)

    def testMakeMatchExceptionName(self):
        """except with named, specified exception type"""
        data_in = {
            'indent': 0,
            'body': 'except ValueError as E:',
            'filename': '',
            'line': 0
        }
        res = ExceptToken.make(data_in)
        self.assertTrue(res)
        self.assertEqual(res.exc_name, 'E')
        self.assertEqual(res.exc_type, 'ValueError')
        self.assertFalse(res.content)

    def testMakeMatchExceptionNameContent(self):
        """except with named, specified exception type"""
        data_in = {
            'indent': 0,
            'body': 'except ValueError as E: "yes"',
            'filename': '',
            'line': 0
        }
        res = ExceptToken.make(data_in)
        self.assertTrue(res)
        self.assertEqual(res.exc_name, 'E')
        self.assertEqual(res.exc_type, 'ValueError')
        self.assertTrue(res.content)
        self.assertEqual(eval(res.content[0], {}, {}), "yes")


class TagTokenTestCase(unittest.TestCase):
    ""

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {'indent': 0, 'body': 'notatag', 'filename': '', 'line': 0}
        res = TagToken.make(data_in)
        self.assertFalse(res)

    def testMakeBareTag(self):
        """tag only - no content or atts"""
        data_in = {'indent': 0, 'body': 'p:', 'filename': '', 'line': 0}
        token = TagToken.make(data_in)
        self.assertTrue(token)
        self.assertEqual(token.tagname, 'p')
        self.assertFalse(token.content)

    def testMakeTagWithContent(self):
        """tag with content but no atts"""
        data_in = {
            'indent': 0,
            'body': 'p: "content"',
            'filename': '',
            'line': 0
        }
        token = TagToken.make(data_in)
        self.assertTrue(token)
        self.assertEqual(token.tagname, 'p')
        self.assertTrue(token.content)
        _globals = {}
        _locals = {}
        content = eval(token.content[0], _globals, _locals)
        self.assertEqual(content, 'content')

    def testMakeTagWithContentAtts(self):
        """tag with content but no atts"""
        data_in = {
            'indent': 0,
            'body': 'p: "content", class="myclass", something=5**2',
            'filename': '',
            'line': 0
        }
        token = TagToken.make(data_in)
        self.assertTrue(token)
        self.assertEqual(token.tagname, 'p')
        self.assertTrue(token.content)
        _globals = {}
        _locals = {}
        content = eval(token.content[0], _globals, _locals)
        self.assertEqual(content, 'content')
        self.assertIn('class', token.atts)
        self.assertEqual(eval(token.atts['something'], _globals, _locals), 25)

    def testSafeKeywords(self):
        "test we can make keywords safe for parsing"
        token = TagToken()
        data_in = 'something=1, class="someclass", for="someid", data-something="else"'
        expected = 'something=1, cls="someclass", for_id="someid", data_something="else"'
        res = token.safeKeywords(data_in)
        self.assertEqual(res, expected)

    def testRestoreKeywords(self):
        "test we can restore keywords post parsing"
        token = TagToken()
        data_in = dict(
            something=1,
            cls="someclass",
            for_id="someid",
            data_something="else")
        expected = ['class', 'data-something', 'for', 'something']
        res = sorted(token.restoreKeywords(data_in).keys())
        self.assertEqual(res, expected)


class CompoundTokenTestCase(unittest.TestCase):
    ""

    def testMakeNoMatch(self):
        """no match - no token"""
        data_in = {'indent': 0, 'body': 'notatag', 'filename': '', 'line': 0}
        res = CompoundToken.make(data_in)
        self.assertFalse(res)

    def testMakeLeast(self):
        """the least possible token"""
        data_in = {'indent': 0, 'body': 'def test:', 'filename': '', 'line': 0}
        res = CompoundToken.make(data_in)
        self.assertTrue(res)

    def testMakeLeastWithContentName(self):
        """the least possible token"""
        data_in = {
            'indent': 0,
            'body': 'def test: __my_content__',
            'filename': '',
            'line': 0
        }
        res = CompoundToken.make(data_in)
        self.assertEqual(res.placeholder, '__my_content__')

    def testMakeLeastWithAtts(self):
        """the least possible token"""
        data_in = {
            'indent': 0,
            'body': 'def test: x=4, y=3',
            'filename': '',
            'line': 0
        }
        res = CompoundToken.make(data_in)
        self.assertEqual(sorted(res.atts.keys()), ['x', 'y'])


class CodeTokenTestCase(unittest.TestCase):
    "test compilation of plain python expression"

    def testGoodExpression(self):
        """compile a line suggessfully"""
        data_in = {'indent': 0, 'body': '"success"', 'filename': '', 'line': 0}
        token = CodeToken.make(data_in)
        _globals = {}
        _locals = {}
        self.assertEqual(eval(token.content[0], _globals, _locals), "success")

    def testGoodExpression2(self):
        """compile a line suggessfully"""
        data_in = {'indent': 0, 'body': '"content"', 'filename': '', 'line': 0}
        token = CodeToken.make(data_in)
        _globals = {}
        _locals = {}
        self.assertEqual(eval(token.content[0], _globals, _locals), "content")

    def testBadExpression(self):
        """compile a line suggessfully"""
        data_in = {'indent': 0, 'body': '!&!$$%', 'filename': '', 'line': 0}
        self.assertRaises(EvoSyntaxError, CodeToken.make, data_in)

    def testAssignment(self):
        """compile an assignment as an exec"""
        data_in = {'indent': 0, 'body': 'x=5', 'filename': '', 'line': 0}
        token = CodeToken.make(data_in)
        _globals = {}
        _locals = {}
        eval(token.content[0], _globals, _locals)
        self.assertEqual(_locals, {'x': 5})

    def testComplexAssignment(self):
        """compile an assignment as an exec"""
        data_in = {
            'indent': 0,
            'body': 'x,y,z=2,3,5',
            'filename': '',
            'line': 0
        }
        token = CodeToken.make(data_in)
        _globals = {}
        _locals = {}
        eval(token.content[0], _globals, _locals)
        self.assertEqual(_locals, {'x': 2, 'y': 3, 'z': 5})


#### Parser ####
class ParserTestCase(unittest.TestCase):
    """common components for parser tests"""

    def setUp(self):
        "create our parser"
        self.parser = EvoParser()
        self.env = {}

    def compare(self, filename, expected=None):
        """compare an evo file with an expected html file.
       Eg.  evo/sometest.evo should match evo/sometest.html
       expected should be a string which takes precedence
       over the html file.
    """
        token = self.parser.parse(filename=filename)
        if expected is None:
            f = open(filename.replace('.evo', '.html'), 'rb')
            expected = str(f.read(), 'utf8')
            f.close()
        res = token(self.env)
        # strip trailing newline
        self.assertEqual(res, expected.rstrip())


class LineGeneratorTestCase(ParserTestCase):
    """line_generator will yield logical python lines.
     For now it only handles plain delimited lines.
  """

    def testNoSource(self):
        "no source = no lines"
        res = list(self.parser.line_generator())
        self.assertEqual(res, [])

    def testEmptySources(self):
        "no source = no lines"
        emptyfile = StringIO()
        res = list(self.parser.line_generator(file=emptyfile))
        self.assertEqual(res, [])
        res = list(self.parser.line_generator(filename='evo/empty.evo'))
        self.assertEqual(res, [])
        res = list(self.parser.line_generator(content=''))
        self.assertEqual(res, [])
        res = list(
            self.parser.line_generator(filename='evo/OneLineComment.evo'))
        self.assertEqual(res, [])

    def testSingleLine(self):
        "a source with a single line"
        res = list(
            self.parser.line_generator(filename='evo/SingleCodeLine.evo'))
        self.assertEqual(len(res), 1)
        self.assertEqual(res[0], {
            'body': '"a single line"',
            'line': 1,
            'indent': 0,
            'filename': './evo/SingleCodeLine.evo'
        })

    def testTwoLinesWithIndent(self):
        "a source with 2 lines, one of which is indented"
        res = list(
            self.parser.line_generator(filename='evo/TagAndCodeLine.evo'))
        self.assertEqual(len(res), 2)

    def testTinyInclusion(self):
        "test that a single line inclusion is properly handled"
        res = list(
            self.parser.line_generator(filename='evo/IncludeMinimal.evo'))
        self.assertEqual(res[0]['body'], 'tinyinclusion.evo')


class EvoParserTestCase(ParserTestCase):
    "Parse evo into trees of tokens"
    expected = [
        ForToken, IfToken, ElifToken, ElseFinallyToken, TagToken, CodeToken,
        TryToken, ExceptToken
    ]

    def testMakeToken(self):
        "make sure each line of an evo file is correctly identified"
        res = []
        lines = self.parser.line_generator(filename='evo/AllTokenTypes.evo')
        for line in lines:
            token = self.parser.make_token(line)
            res.append(token.__class__)
        self.assertEqual(res, self.expected)

    def testParseAllTokenTypes(self):
        "parse a file.  No indentation."
        token = self.parser.parse(filename='evo/AllTokenTypes.evo')
        self.assertEqual(len(token.content), len(self.expected))

    def testParseAllTokenTypesIndented(self):
        "parse a file.  Has indentation."
        token = self.parser.parse(filename='evo/AllTokenTypesIndented.evo')
        self.assertEqual(len(token.content), 1)

    def testFindFileNotFound(self):
        ""
        self.assertRaises(
            EvoPathError,
            self.parser.parse,
            filename='evo/ThisFileDoesNotExist.evo')

    def testFindFileFoundInOtherPath(self):
        ""
        parser = EvoParser(paths=['.', '..', 'evo'])
        token = parser.parse(filename='AllTokenTypesIndented.evo')
        self.assertTrue(token)

    def testTinyIncluded(self):
        "make sure we handle a tiny inclusion properly"
        token = self.parser.parse(filename='evo/tinyinclusion.evo')

    def testTinyInclusion(self):
        "make sure we handle a tiny inclusion properly"
        token = self.parser.parse(filename='evo/IncludeMinimal.evo')


#### Render ####


class TokenRenderTestCase(ParserTestCase):
    "render single token types and evo documents"

    def testRenderCodeOnly(self):
        "render a single string"
        token = self.parser.parse(content='"content"')
        res = token(self.env)
        self.assertEqual(res, "content")

    def testRenderCodeOnlyNumber(self):
        "render a single number"
        token = self.parser.parse(content='1012')
        res = token(self.env)
        self.assertEqual(res, "1012")

    def testRenderCodeOnlyFunction(self):
        "render a single number"
        token = self.parser.parse(content='sum(i**2 for i in range(4))')
        res = token(self.env)
        self.assertEqual(res, "14")

    # Nb. Singleton tags aka void elements are a small subset of html5

    def testRenderTagNoAtts(self):
        "a singleton tag"
        token = self.parser.parse(content='img:')
        res = token(self.env)
        self.assertEqual(res, '<img/>')

    def testRenderTagAtts(self):
        "a singleton tag with atts"
        token = self.parser.parse(content='img: zoo="a zoo", class="usual"')
        res = token(self.env)
        self.assertEqual(res, '<img class="usual" zoo="a zoo" />')

    def testRenderTagSingleAtts(self):
        "a singleton tag with single atts"
        token = self.parser.parse(content='img: disabled=False, selected=True')
        res = token(self.env)
        self.assertEqual(res, '<img selected="selected" />')

    def testRenderTagSimpleContent(self):
        "a tag with simple content"
        token = self.parser.parse(content='tag: "content"')
        res = token(self.env)
        self.assertEqual(res, '<tag>content</tag>')

    def testRenderTagNestedContent(self):
        "a tag with simple content"
        token = self.parser.parse(filename='evo/NestedTags.evo')
        res = token(self.env)
        self.assertEqual(res, '<div><p>content<span>odd</span></p></div>')

    def testRenderTagNestedContentInline(self):
        "a tag with simple content"
        content = """
div: 
  p: "content"
    span: "odd"
"""
        token = self.parser.parse(filename='evo/NestedTags.evo')
        res = token(self.env)
        self.assertEqual(res, '<div><p>content<span>odd</span></p></div>')


class ForRenderTestCase(ParserTestCase):
    """render for loops"""

    def testRenderForSimpleIterationSimpleContent(self):
        token = self.parser.parse(filename='evo/SimpleFor.evo')
        res = token(self.env)
        self.assertEqual(res, 'content-1content-2content-3')

    def testRenderForSimpleIterationSimpleContent(self):
        ""
        token = self.parser.parse(filename='evo/SimpleForNestedTags.evo')
        res = token(self.env)
        self.assertEqual(
            res,
            '<p>content-1<span>.</span></p><p>content-2<span>.</span></p><p>content-3<span>.</span></p>'
        )

    def testRenderForSimpleIterationSimpleContentUsingCompare(self):
        "same as above but using our compare method"
        self.compare('evo/SimpleForNestedTags.evo')

    def testRenderEmptyForWithElse(self):
        "same as above but using our compare method"
        self.compare('evo/EmptyForWithElse.evo')


class IfRenderTestCase(ParserTestCase):
    """Render If"""

    def testRenderIfOnly(self):
        "an if that is true"
        self.compare('evo/IfOnly.evo')

    def testRenderIfElseTrue(self):
        "an if that is true"
        self.compare('evo/IfElseTrue.evo')

    def testRenderIfElseFalse(self):
        "an if that is false"
        self.compare('evo/IfElseFalse.evo')

    def testRenderIfElifBothFalse(self):
        "the if and elif are both false"
        self.compare('evo/IfElifBothFalse.evo', expected='')

    def testRenderIfElifIfTrue(self):
        "the if statement is true, the elif is false"
        self.compare('evo/IfElifIfTrue.evo', expected='yes')

    def testRenderIfElifTrue(self):
        "the if statement is false, the elif is true"
        self.compare('evo/IfElifIfTrue.evo', expected='yes')

    def testRenderIfElifBothTrue(self):
        "the if statement is true, the elif is true"
        self.compare('evo/IfElifBothTrue.evo', expected='yes')

    def testRenderIfElifElseBothFalse(self):
        "the if statement is false, the elif is false"
        self.compare('evo/IfElifElseBothFalse.evo', expected='yes')

    def testRenderIfElifElseIfTrue(self):
        "the if statement is false, the elif is false"
        self.compare('evo/IfElifElseIfTrue.evo', expected='yes')

    def testRenderIfElifElseElifTrue(self):
        "the if statement is true, the elif is false"
        self.compare('evo/IfElifElseElifTrue.evo', expected='yes')

    def testRenderIfElifElseBothTrue(self):
        "the if statement is true, the elif is true"
        self.compare('evo/IfElifElseBothTrue.evo', expected='yes')

    def testRenderIfElifElseComplex(self):
        "stack of nested if statements"
        self.compare('evo/IfElifElseComplex.evo', expected='yes')


class IfRenderTestCase(ParserTestCase):
    """Render Inclusion"""

    def testMakeBareInclude(self):
        """Include only"""
        data_in = {
            'indent': 0,
            'body': 'evo/tinyinclusion.evo',
            'filename': '',
            'line': 0
        }
        parser = EvoParser()
        token = IncludeToken.make(data_in, parser)
        self.assertTrue(token)
        self.assertEqual(token.__class__, IncludeToken)

    def testRenderTinyInclusion(self):
        "empty including file, minimal inclusion"
        self.compare('evo/tinyinclusion.evo', expected='yes')

    def testRenderTinyInclude(self):
        "empty including file, minimal inclusion"
        self.compare('evo/IncludeMinimal.evo', expected='yes')

    def testIncludeComplexNoContext(self):
        "complex inclusion, no content in including file"
        self.compare('evo/IncludeComplexNoContext.evo')

    def testIncludeComplexContext(self):
        "complex inclusion, content in including file"
        self.compare('evo/IncludeComplexContext.evo')

    def testIncludeComplexStack(self):
        "complex inclusion, content in including file"
        self.compare('evo/IncludeStackOuter.evo')

    def testIncludeWithContext(self):
        """Pass context to included file.  
       This overrides self which must be retained in
       the outer context
    """
        self.compare('evo/IncludeWithContext.evo')


class TrySiblingTestCase(ParserTestCase):
    """Test TryToken.siblings"""

    def testTryOnly(self):
        """a try without an except is not strictly valid python 
       but currently ok in this content
    """
        token = self.parser.parse(filename='evo/TryOnly.evo')
        trytoken = token.content[0]
        self.assertEqual(trytoken.siblings(),
                         {'exc': [],
                          'else': None,
                          'finally': None})

    def testTryExcept(self):
        """ try and single except
    """
        token = self.parser.parse(filename='evo/TryExcept.evo')
        trytoken = token.content[0]
        res = trytoken.siblings()
        self.assertEqual(len(res['exc']), 1)
        self.assertFalse(res['else'])
        self.assertFalse(res['finally'])

    def testTryMultiExcept(self):
        """ try and multiple excepts and else
    """
        token = self.parser.parse(filename='evo/TryMultiExcept.evo')
        trytoken = token.content[0]
        res = trytoken.siblings()
        self.assertEqual(len(res['exc']), 2)
        self.assertFalse(res['else'])
        self.assertFalse(res['finally'])

    def testTryExceptElse(self):
        """ try and single except and else
    """
        token = self.parser.parse(filename='evo/TryExceptElse.evo')
        trytoken = token.content[0]
        res = trytoken.siblings()
        self.assertEqual(len(res['exc']), 1)
        self.assertTrue(res['else'])
        self.assertFalse(res['finally'])

    def testTryExceptFinally(self):
        """ try and single except and finally
    """
        token = self.parser.parse(filename='evo/TryExceptFinally.evo')
        trytoken = token.content[0]
        res = trytoken.siblings()
        self.assertEqual(len(res['exc']), 1)
        self.assertFalse(res['else'])
        self.assertTrue(res['finally'])

    def testTryExceptElseFinally(self):
        """ try and single except, else and finally
    """
        token = self.parser.parse(filename='evo/TryExceptElseFinally.evo')
        trytoken = token.content[0]
        res = trytoken.siblings()
        self.assertEqual(len(res['exc']), 1)
        self.assertTrue(res['else'])
        self.assertTrue(res['finally'])

    def testTryExceptElseFinallyTrailing(self):
        """ try and single except and finally with trailing content to be ignored
    """
        token = self.parser.parse(
            filename='evo/TryExceptElseFinallyTrailing.evo')
        trytoken = token.content[0]
        res = trytoken.siblings()
        self.assertEqual(len(res['exc']), 1)
        self.assertTrue(res['else'])
        self.assertTrue(res['finally'])


class TryRenderTestCase(ParserTestCase):
    ""

    def testRenderTryOnly(self):
        self.compare('evo/TryOnly.evo', expected='yes')

    def testRenderTryExceptSuccess(self):
        self.compare('evo/TryExcept.evo', expected='yes')

    def testRenderTryExceptError(self):
        self.compare('evo/TryExceptError.evo', expected='yes')

    def testRenderTryExceptElseSuccess(self):
        self.compare('evo/TryExceptElseSuccess.evo', expected='yeselse')

    def testRenderTryExceptElseError(self):
        self.compare('evo/TryExceptElseError.evo', expected='yes')

    def testRenderTryExceptFinally(self):
        self.compare('evo/TryExceptFinally.evo', expected='finally')

    def testRenderTryExceptFinallyError(self):
        self.compare('evo/TryExceptFinallyError.evo', expected='yesfinally')

    def testRenderTryMultiExceptDirectMatch(self):
        self.compare('evo/TryExceptMultiExceptDirectMatch.evo', expected='yes')

    def testRenderTryMultiExceptNoDirectMatch(self):
        self.compare(
            'evo/TryExceptMultiExceptNoDirectMatch.evo', expected='yes')

    # Not relevant now we use ExceptionContainers    
#    def testRenderTryExceptUncaught(self):
#        self.assertRaises(
#            ZeroDivisionError,
#            self.compare,
#            'evo/TryExceptUncaught.evo',
#            expected='yes')

    def testRenderTryNested(self):
        self.compare('evo/TryNested.evo', expected='yes')

    def testRenderTryNestedFinally(self):
        self.compare('evo/TryNestedFinally.evo', expected='yes')

    def testRenderTryMultiNestedFinally(self):
        self.compare(
            'evo/TryMultiNestedFinally.evo', expected='onetwothreefourfivesix')


class TokenFilenameTestCase(ParserTestCase):
    """ Token.filenames returns a list of filenames
      used in the token and its content
  """
    #### TODO Test something


class CompoundRenderTestCase(ParserTestCase):
    ""

    def testMakeInContext(self):
        """create a token and find it in its parent namespace"""
        token = self.parser.parse(filename='evo/CompoundInContext.evo')
        # we should see an empty div - TODO make it appear to be a singleton
        self.assertEqual(token({}), '<head/>')

        # look for the compound tag in the div's namespace
        self.assertIn('compound', token.content[0].namespace)

        # render the compound tag
        self.assertEqual(token.content[0].namespace['compound']({}),
                         '<div>compound</div>')

    def testDefineAndRenderLeast(self):
        """define a compound function and render it"""
        self.compare(
            'evo/CompoundDefineAndRenderLeast.evo',
            expected='<div>compound</div>')

    def testCompoundDefineRenderWithParameters(self):
        """compound tag with default parameters and content"""
        self.compare('evo/CompoundDefineRenderWithParameters.evo')
        #token = self.parser.parse(filename='evo/CompoundDefineRenderWithParameters.evo')

    def testCompoundSeparateFile(self):
        """keep our compound definitions in a separate file"""
        global token
        token = self.parser.parse(filename='evo/CompoundSeparateFile.evo')
        self.compare('evo/CompoundSeparateFile.evo')

    def testNestedCompound(self):
        """Test token.filenames"""
        self.compare('evo/IncludeNestOuter.evo')
        token = self.parser.parse(filename='evo/IncludeNestOuter.evo')
        print(token.filenames())
        self.assertEqual(
            set(token.filenames()),
            set([
                '', './evo/IncludeNestOuter.evo', './evo/IncludeNestMid.evo',
                './evo/IncludeNestInner.evo'
            ]))

    def testLastUpdate(self):
        """test for most recent update in a compound file.

       TODO: generate evo files, measure the mtime, change one then remeasure.
    """
        # copy the IncludeNest set

        # record last_update for IncludeNestOuter.evo
        token = self.parser.parse(filename='evo/IncludeNestOuter.evo')
        original_last_update = token.last_update()
        self.assertTrue(original_last_update)

        # modify IncludeNestInner.evo
        f = open('evo/IncludeNestInner.evo', 'rb')
        s = f.read()
        f.close()

        f = open('evo/IncludeNestInner.evo', 'wb')
        f.write(s)
        f.close()

        # record revised last_update for IncludeNestOuter
        token = self.parser.parse(filename='evo/IncludeNestOuter.evo')
        new_last_update = token.last_update()
        self.assertTrue(new_last_update > original_last_update)


class WrapperTestCase(ParserTestCase):
    """It should be possible to store a token as a variable in env
     then call it at runtime.
  """

    def testSimpleContent(self):
        """include a token in env"""
        wrapper = self.parser.parse(filename='evo/wrapper1.evo')
        content = self.parser.parse(filename='evo/simple_content.evo')
        env = {'__content__': content}
        self.assertEqual(wrapper(env), '<div>yes</div>')

     # TODO: reinstate - find original versions of wrapper1.evo, wrapper2.evo
#    def testSimpleContent(self):
#        """include a token in env"""
#        wrapper = self.parser.parse(filename='evo/wrapper1.evo')
#        content = self.parser.parse(filename='evo/simple_content.evo')
#        env = {'__content__': content}
#        self.assertEqual(wrapper(env), '<div>yes</div>')


    def testSimpleContentWithConvienceFunction(self):
        """include a token in env - no need for locals() in evo"""
        wrapper = self.parser.parse(filename='evo/wrapper2.evo')
        content = self.parser.parse(filename='evo/simple_content.evo')

        def contentfn():
            return content(locals())

        env = {'__content__': contentfn}
        self.assertEqual(wrapper(env), '<div>yes</div>')

     # TODO: reinstate - find original versions of wrapper1.evo, wrapper2.evo
#    def testSimpleContentWithConvienceFunctionAndEnv(self):
#        """include a token in env - no need for locals() in evo"""
#        wrapper = self.parser.parse(filename='evo/wrapper2.evo')
#        content = self.parser.parse(filename='evo/simple_content_uses_env.evo')
#        env = {'x': 'yes'}
#
#        def contentfn():
#            return content(locals())
#
#        env['__content__'] = contentfn
#        self.assertEqual(wrapper(env), '<div>yes</div>')


class NevoTemplateTestCase(ParserTestCase):
    """ 
  """

    def testCache(self):
        """ensure we always get the latest version of a token"""
        filename = 'simple_content_uses_env.evo'
        wfilename = 'wrapper2.evo'
        nt = NevoTemplate(filename, wfilename)
        wrapper = nt.fetch(wfilename)
        token = nt.fetch(filename)

        # trigger newer version of inner template
        fn = 'evo/%s' % filename
        f = open(fn, 'rb')
        s = f.read()
        f.close()

        f = open(fn, 'wb')
        f.write(s)
        f.close()

        # wrapper should stay the same
        self.assertEqual(wrapper, nt.fetch(wfilename))

        # token should have changed
        self.assertNotEqual(token, nt.fetch(filename))


class FieldTemplateTestCase(ParserTestCase):
    """test for issues seen in use"""       

    def testStackOfScript(self):
        """multiple scripts as self closing elements, one after the other"""
        self.compare('evo/StackOfScript.evo')

#    def testStackedIncludes(self):
#        """This test will fail.  The issue was that the script tags
#           in the include tags stopped the html being displayed
#           in Firefox.  This is because singleton <script />
#           tags still break it.
#        """
#        self.maxDiff = None  # show full output on error
#        self.compare('wraptest/wrapper.evo')

if __name__ == '__main__':
    unittest.main()
