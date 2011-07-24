#!/usr/local/bin/python

"""Extensible Markup Language Scanner, Checker, and Utilities

This xml module includes the followin classes:
  Scanner          -- handles XML syntax only: no well-formedness nor
                      validity constraints
  WellFormed       -- scanner client that
                      well-formedness constraints

  InferEndTags     -- a WellFormed subclass that overrides
                      start tag processing to do a limited
              form of end-tag inference that is
              sufficient for HTML 3.2

See: Extensible Markup Language
     http://www.w3.org/TR/WD-xml-lang

This implementation is not quite complete.
There are also some differences from the spec.
@@ marks things I intend to fix. The other differences
are things I probably want to change in the XML spec.
@# marks things that I'm still not comfortable with.

This module also includes a test harness.
 Usage: pyton xml.py [-emit start|end|empty|comment|eref|cref|...]+
                     [-all] < entity


Copyright (c) 1997 by Massachusetts Institute of Technology (MIT),
                      INRIA,
                      Keio
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee or royalty is hereby granted,
as per <http://www.w3.org/pub/WWW/COPYRIGHT>.

written by Dan Connolly <connolly@w3.org> http://www.w3.org/People/Connolly/
$Id: xml.py,v 1.8 1997/08/11 02:45:25 connolly Exp $
"""

import regex
from string import lower, find, atoi, index, count

#############################################################
# The Client class and supporting utilities, exceptions, etc.
#

Result = 'xml.Done'

Lit        = 'lit'          # quoted literals: "dsj" or 'sdfsd'
Symbol     = 'symbol'       # symbol, e.g. text in <input type=text>
Number     = 'number'       # numbers, e.g. 10 in <img width=10>
                            #@# hmm... float, cardinal, int, units?
Enum       = 'enum'         # enumerated/booleans, e.g. ismap in <img ismap>

SpacePat    = '[ \t\r\n]+'  #@@ unicode space?

class Client:
    def __init__(self):
    pass

    ###########
    # Methods to override to handle parsed data
    #
    # raise Result to tell the scanner to pause
    # and return a result
    #

    def text(self, str):
    pass

    def openStart(self, name):
    pass

    def attribute(self, name, type, value):
    pass

    def closeStart(self):
    pass

    def closeEmpty(self):
    pass

    def endTag(self, name=None):
    pass

    def comment(self, stuff):
    pass

    def pi(self, stuff):
    pass

    def decl(self, name, parts):
    pass

    def cref(self, numeral):
    pass

    def eref(self, name):
    pass

    def eof(self):
    pass


def fixcase(str):
    """Normalize the case of names and symbols."""
    return lower(str)

Entities = {'lt': '<', 'gt': '>', 'amp': '&',
        'quot': '"', 'apos': "'"}

def evalLit(str, replacements = Entities):
    """Evaluate an XML literal.

    raises index error if ; missing, key error if undefined entity"""

    return unescape(str[1:-1], replacements)

MarkupDelimChars = (('<', '&lt;'), ('>', '&gt;'), ('&', '&amp;'))
LitDelimChars = (('"', '&quot;'), ("'", '&apos;'), ('&', '&amp;'))

def escape(str, replacements = MarkupDelimChars):
    """remember to put & last!"""
    for pat, repl in replacements:
    str = regsub.gsub(pat, repl, str)
    return str

def unescape(str, replacements = Entities):
    """raises index error if ; missing, key error if undefined entity"""
    out = ''
    while 1:
    i = find(str, '&')
    if i < 0:
        return out + str
    else:
        out = out + str[:i]
        str = str[i+1:]
        j = index(str, ';')
        out = out + deref(str[:j], replacements)
        str=str[j+1:]

def deref(str, replacements = Entities):
    """raises index error if ; missing, key error if undefined entity"""
    if str[0] == '#':
    return chr(atoi(str[1:])) #@@ x, hex
    if str[:2] == 'U-':
    return chr(atoi(str[2:], 16))
    else:
    return replacements[str]

def ScanErrorMessage(outfp, info):
    message, line, pending, offending = info
    outfp.write(("ERROR: %d: %s\n" + \
         "context: %s{!!!}%s\n" ) % \
        (line, message, pending, offending))

def NotWellFormedMessage(outfp, info, wfc):
    message, line, offending = info
    s = wfc.stack
    if s:
    name, ln = s[-1].name, s[-1].line
    ref = "(see <%s> line: %d)" % (name, ln)
    else:
    ref = ''
    outfp.write(("%d: %s %s\n" + \
         "bad text: %s\n") % \
        (line, message, ref, offending))
    for elm in wfc.stack:
    outfp.write(" line %d: <%s[%d] %s>\n" %
            (elm.line, elm.name, elm.idx, `elm.attrs`))



ScanError = 'xml.ScanError'

class Scanner:
    ###########
    # Exported Scanner methods

    def __init__(self):
    self._line = 1
    self.done = -1
    self.bb = self.at = 0
    self._buf = ''

        # #@# this scanner doesn't signal an error when ]]> occurs
        # outside a marked section
    self.RE = union(('[^&<]+',
             '<!--', '<!\[', '<!', '<\?',
             '</', '<',
             '&#', '&'))

    self.choices =   (self.noDelim,
              self.commentDelim, self.sectionDelim,
                self.declDelim, self.piDelim,
              self.endDelim, self.startDelim,
              self.crefDelim, self.erefDelim)

    self.numberRE   = regex.compile('[0-9]+')
    self.spaceRE    = regex.compile(SpacePat)
    self.litRE      = regex.compile("'[^']*'\|\"[^\"]*\"")
    self.symbolRE   = regex.compile('[a-zA-Z0-9\._-]+')
    self.nameRE     = regex.compile('[a-zA-Z][a-zA-Z0-9\._-]*')


    def client(self, cl):
    #@# multiple clients?
    #@# event masks?
    self.cl = cl

    def feed(self, text):
        self._buf = self._buf + text

    # @# use client=None to refer to the default client which
    #  returns the construct in a canonical form
    def next(self, client):
    while self.at < len(self._buf):
        i = self.RE.match(self._buf, self.at)
        if i > 0:
        self.at = self.at + i
        regs = self.RE.regs
        for i in range(0, len(self.choices)):
            if regs[i+1][1] > 0:
            proc = self.choices[i]
            break

        try:
            proc(client)
        except Result, r:
            return r

        else: raise ScanError, ('illegal delimiter',
                    self._line, '',
                    self._buf[self.at:self.at+25])


    def line(self):
    return self._line

    def raw(self):
    """get raw text of last construct parsed"""
    return self._buf[self.done:self.bb]

    ###########
    # internal Scanner methods

    def consume(self, more=0):
    at = self.at + more
    self._line = self._line + count(self._buf[:at], '\n', self.bb)
    self.done = self.bb
    self.bb = self.at = at

    def noDelim(self, client):
    self.consume()
    client.text(self.raw())
    
    def name(self, case=0):
        i = self.nameRE.match(self._buf, self.at)
    if i > 0:
        name = self.nameRE.group(0)
        self.at = self.at + i
        if case: name = fixcase(name)
        return name
    else:
        return None

    def space(self):
        i = self.spaceRE.match(self._buf, self.at)
    if i > 0:
        self.at = self.at + i

    def startDelim(self, client):
        name = self.name(1)
    if name:
        client.openStart(name)

        while 1:
        self.space()

            if self._buf[self.at:self.at+2] == '/>':
            self.consume(2)
            client.closeEmpty()
            break

            elif self._buf[self.at] == '>':
            self.consume(1)
            client.closeStart()
            break
        elif self.at < len(self._buf):
                self.attribute(client)
            else:
            self.err('expected /> or > at end of tag')
    else:
        self.err('expected tag name after <')

    def attribute(self, client):
    name = self.name()

    if not name:
        self.err('expected attribute name')

    name = fixcase(name)
        self.space()

    if self._buf[self.at] == '=':
        self.at = self.at + 1
        self.space()

        i = self.litRE.match(self._buf, self.at)
        if i > 0:
            lit = self.litRE.group(0)
        self.consume(i)
        client.attribute(name, Lit, lit)
        #@# else if number...
            else:
        i = self.symbolRE.match(self._buf, self.at)
            if i > 0:
            sym = self.symbolRE.group(0)
            self.consume(i)
            client.attribute(name, Symbol, sym)
        else:
            self.err('expected attribute value after =')
        else:
        self.consume()
        client.attribute(name, Enum, name)


    def endDelim(self, client):
        name = self.name(1)
    if name:
        self.space()
        if(self._buf[self.at] == '>'):
            self.consume(1)
        client.endTag(name)
        else:
            self.err('expected > after end tag name')
        elif self._buf[self.at] == '>':
        self.consume(1)
        client.endTag()
        else:
        self.err('expected end tag name after </')

    def commentDelim(self, client):
    #@# hmmm... python regex module seems to have a limitation
        # a comment of about 3k chars raised a regex.error
        i = find(self._buf, '--', self.at)
    if i>0:
        if self._buf[i+2] == '>':
        stuff = self._buf[self.at:i]
        self.consume(i + 3 - self.at)
        client.comment(stuff)
        else:
        self.err('bad comment')
    else:
        self.err('bad comment')

    def piDelim(self, client):
        i = find(self._buf, '>', self.at)
    if i>0:
        stuff = self._buf[self.at:i]
        self.consume(i + 1 - self.at)
        client.pi(stuff)
    else:
        self.err('bad processing instruction')

    def declDelim(self, client):
    name = self.name()
    if not name:
        self.err('expected declaration keyword after <!')

    name = fixcase(name)

    parts = []

    while 1:
        self.space()

        if self._buf[self.at] == '>':
        self.consume(1)
        client.decl(name, parts)
        break
        else:
        i = self.litRE.match(self._buf, self.at)
        if i > 0:
            lit = self.litRE.group(0)
            self.at = self.at + i
            parts.append((Lit, lit))
        #@# else if number...
        else:
            i = self.symbolRE.match(self._buf, self.at)
            if i > 0:
            tok = self.symbolRE.group(0)
            self.at = self.at + i
            parts.append((Symbol, tok))
            else:
            self.err('bad declaration')

    def sectionDelim(self, client):
    #@# add support for <![xyz[ ... ]xyz]>
        i = find(self._buf, ']]>', self.at)
    if i>0:
        stuff = self._buf[self.at:i]
        self.consume(i + 3 - self.at)
        client.text(stuff)
    else:
        self.err('bad marked section')


    def crefDelim(self, client):
    # @@ unicode &$x
        i = self.numberRE.match(self._buf, self.at)
    if i > 0:
        num = self.numberRE.group(0)
        if self._buf[self.at + i] == ';':
            self.consume(i+1)
            client.cref(num)
        else:
            self.err('expected ; after character number')
        else:
        self.err('expected character number after &#')

    def erefDelim(self, client):
        name = self.name()
        if name:
        if self._buf[self.at] == ';':
            self.consume(1)
            client.eref(name)
        else:
            self.err('expected ; after entity name')
        else:
        self.err('expected entity name after &')

    def err(self, msg):
        raise ScanError, (msg,
              self._line,
              self._buf[self.bb:self.at],
              self._buf[self.at:self.at+25])


def union(choices):
    exp = ''
    for sub in choices:
    if exp: exp = exp + '\|'
    exp = exp + '\(' + sub + '\)'
    return regex.compile(exp)



NotWellFormed = 'xml.NotWellFormed'

#@@ split client interface in two. Clean up empty=start/end

class WellFormed(Client):
    """XML scanner client that enforces well-formedness constraints

    Also maintains a stack of open elements."""

    def __init__(self):
    Client.__init__(self)
        self.stack = []
    self.element = 1
    self.elm = None
    self.spaceRE = regex.compile(SpacePat)

    def startElement(self, elm):
    pass

    def endElement(self, elm):
    pass

    def scanner(self, s):
    """Tell self which scanner to call to get line numbers and raw input."""
    self.p = s

    def text(self, str):
    # @# hmmm... I think this should be a VC, not a WFC
    if not self.stack:
        if self.spaceRE.match(str) != len(str):
        raise NotWellFormed, ('data outside root element',
                      self.p.line(), self.p.raw())

    def cref(self, numeral):
    self.text(deref('#' + numeral))

    def eref(self, name):
    try:
        t = deref(name)
    except KeyError:
        #@@ Don't silently fix errors!
        self.text(self.p.raw())
        return
    self.text(t)

    def openStart(self, name):
    self.elm = Element(name, self.element, self.p.line())

    def attribute(self, name, type, value):
    if self.elm.attrs.has_key(name):
        raise NotWellFormed, ('duplicate attribute name',
                  self.p.line(), name)
    if type == Lit:
        try:
        self.elm.attrs[name] = evalLit(value)
        except KeyError:
        print "@@unknown entity", value

    else: self.elm.attrs[name] = value

    def closeStart(self):
    self.stack.append(self.elm)
    self.startElement(self.elm)
    self.element = self.element + 1

    def closeEmpty(self):
    self.stack.append(self.elm)
    self.startElement(self.elm)
    self.endElement(self.elm)
    del self.stack[-1]

    def endTag(self, name = None):
    if len(self.stack) == 0:
        raise NotWellFormed, ('extra end tag',
                  self.p.line(), self.p.raw())

    elm = self.stack[-1]
    if name is None or name == elm.name:
        self.endElement(elm)
        del self.stack[-1]
    else:
        raise NotWellFormed, ('end tag mismatch',
                  self.p.line(), self.p.raw())

    #@# check declarations. Hmmm... VC or WFC?

    def eof(self):
    if len(self.stack) > 0:
        raise NotWellFormed, ('not enough end tags', self.p.line(), '')


class Element:
    def __init__(self, name, idx, line):
    self.name, self.line = name, line
    self.attrs = {}
    self.idx = idx



class InferEndTags(WellFormed):
    """This subclass of WellFormed does rudimentary end-tag inference.
    It can process empty elements, given a list of their names,
    and given a mapping from elements to their "follow" set
    (i.e. the set of elements which cannot occur in content), it does
    end-tag inference.

    @#It works for HTML 3.2, but I haven't considered the general case
    carefully. I wonder if there is a class of documents including
    HTML 3.2 which is smaller than the general case but still interesting.
    """

    def __init__(self):
    WellFormed.__init__(self)
    self.inferEnds = {}
    self.empties = []

    # @# should have openStart() method with Element subclass

    def closeStart(self):
    elm = self.elm
    name = elm.name
    while self.stack:
        if name in self.stack[-1].follow:
        self.endTag()
        else:
        break

    if self.inferEnds.has_key(name):
        elm.follow = self.inferEnds[name]
    else:
        elm.follow = []

    if elm.name in self.empties:
        WellFormed.closeEmpty(self)
    else:
        WellFormed.closeStart(self)

    def endTag(self, name = None):
    if name:
        while self.stack:
        top = self.stack[-1]
        if top.follow and top.name != name:
            WellFormed.endTag(self, None)
        else:
            break

    WellFormed.endTag(self, name)

    def html32(self):
    self.empties = [
        'link', 'meta', 'base',
        'br', 'hr',
        'img', 'param',
        'input', 'option', 'isindex'
        ]
    list = ('ol', 'ul', 'dl')
    heading = ('h1', 'h2', 'h3', 'h4', 'h5', 'h6')
    blocks = ('p', 'address', 'blockquote', 'form', 'table', 'pre') + \
         heading + list
    self.inferEnds = {
        'p': blocks,
        
        'li': ('li',),
        'dt': ('dt',),
        'dd': ('dt', 'dd'),

        'tr': ('tr',),
        'th': ('th', 'td', 'tr'),
        'td': ('th', 'td', 'tr'),

        }



###############
# Test harness
#

import sys

Text       = 'text'         # text data characters
Start      = 'start'        # start element
CloseStart = 'closestart'   # start tag close
CloseEmpty = 'empty'        # empty tag
End        = 'end'          # end element
EndTag     = 'endtag'       # end element
ERef       = 'eref'         # entity reference: &foo;
CRef       = 'cref'         # character reference: &#100;
Comment    = 'comment'      # <!-- bla bla -->
Section    = 'section'      # <![CDATA[ bla bla ]]>
Decl       = 'decl'         # <!doctype ...>
PI         = 'pi'           # <?...>

Events = {Text:Text,
      Start:Start, End:End,
      CloseStart:CloseStart, EndTag:EndTag, CloseEmpty: CloseEmpty, 
      ERef:ERef, CRef:CRef,
      Comment:Comment, Decl:Decl, PI:PI, Section:Section}

class Tester(InferEndTags):
    def __init__(self):
    InferEndTags.__init__(self)
    self.mask = []

    def showEvents(self, mask):
    self.mask = mask

    def process(self, type, info):
    text = self.p.raw()
    if self.mask is None or type in self.mask:
        print "==>%s: %s [%s]\n" % (type, `info`, text)

    def text(self, text):
    InferEndTags.text(self, text)
    self.process(Text, None)

    def closeStart(self):
    InferEndTags.closeStart(self)
    elm = self.elm
    self.process(CloseStart, (elm.name, elm.idx, elm.line, elm.attrs))

    def startElement(self, elm):
    names = []
    for e in self.stack:
        names.append(e.name)

    self.process(Start, (names, elm.idx, elm.line, elm.attrs))

    def closeEmpty(self):
    InferEndTags.closeEmpty(self)
    elm = self.elm
    self.process(CloseEmpty, (elm.name, elm.idx, elm.line, elm.attrs))

    def endTag(self, name = None):
    InferEndTags.endTag(self, name)
    self.process(EndTag, name)

    def endElement(self, elm):
    self.process(End, elm.name)

    def cref(self, num):
    InferEndTags.cref(self, num)
    self.process(CRef, num)

    def eref(self, num):
    InferEndTags.eref(self, num)
    self.process(ERef, num)

    def comment(self, str):
    InferEndTags.comment(self, str)
    self.process(Comment, str)

    def pi(self, str):
    InferEndTags.pi(self, str)
    self.process(PI, str)

    def decl(self, name, parts):
    InferEndTags.decl(self, name, parts)
    self.process(Decl, (name, parts))

    def section(self):
    InferEndTags.section(self, str)
    self.process(Section, None)


def main(infp):
    p = Scanner()
    cl = Tester()
    cl.scanner(p)
    getOpts(cl)

    try:
        p.feed(infp.read())
    p.next(cl)
    cl.eof()

    except ScanError, info:
    ScanErrorMessage(sys.stderr, info)
    sys.exit(1)
    except NotWellFormed, info:
    NotWellFormedMessage(sys.stderr, info, cl)
    sys.exit(1)


def getOpts(x):
    mask = []

    while len(sys.argv) > 1:
        opt = sys.argv[1]
    if opt[0] != '-': break
    del sys.argv[1]

    if opt == '-emit':
        mask.append(Events[sys.argv[1]])
        del sys.argv[1]

    elif opt == '-all':
        mask = None
    elif opt == '-html':
        x.html32()

    else:
        sys.stderr.write("unkonwn option: %s\n" % opt)
        sys.exit(1)

    x.showEvents(mask)


if __name__ == '__main__':
    main(sys.stdin)
