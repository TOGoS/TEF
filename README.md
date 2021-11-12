# TOGoS's Entry Format (TEF) version 0.2.0

A file format designed to store a series of objects in flexible and human and machine-readable way,
inspired by Perl POD and MIME.

Useful when you want to record facts, thoughts, logs, or conceptual objects
in a linear way for later processing, or just want a humand readable/writable
format for data that's simpler than YAML but easier on the eyes than XML or JSON.

Some details regarding edge cases as syntax-level metadata are still in flux,
but the basic syntax (when no escaping or multiline header values are needed)
has been pretty stable since I started using it in the early '00s.

A short, concrete example, showing an implicit first entry (not opened with a "`=`" line),
comments in headers, multi-line headers, and escaped "`=`" lines:

```
#!/usr/bin/env some-tef-processor
# I put a shebang line there just to show that you can.
# Header lines starting with '# ' or '#!' are ignored.
title: An example TEF file
# The following blank line marks the end of the headers,
# and the beginning of tef:content, if any:

This is the content of the implicit file-level entry.

=journal-entry 2021-11-04
title: Feeling gr8!
# A comment line, which is ignored by the TEF parser
tef:content-type: text/plain

Wow today is amazing.


=journal-entry 2021-11-11
title: Escaping equal signs in TEF files
note: You can split header values
  across multiple lines by starting later lines
  with whitespace.
note: You can also have multiple headers of the same name.
==note: Two equal signs at the beginning of a line escape a single "=".
  This header's key is "=note".
tef:content-type: text/plain

The following starts with a single equal sign and is part of this entry's content:
== <- an equal sign.
Wow, now I can make nested TEF files!
# This line is also part of the entry's content, even though it starts with #
```

I've been using this format for over a decade for various purposes and have written many
ad-hoc parsers in that time.
Maybe it's time to write a specification and librarify the parser(s).

Some examples:
- [stuff.tef](./stuff.tef), which is a little bit meta
- [music.txt](http://www.nuke24.net/music/music.txt),
  which is used to generate [the music page on my website](http://www.nuke24.net/music/).
- My [project log entries](http://www.nuke24.net/plog/entries/) are also written
  in this format, but with each file being a self-contained entry.
- I use it extensively in my [project notes](https://gitlab.com/TOGoS/ProjectNotes2/)
  to record e.g. [which stains were used on which pieces of wood](https://gitlab.com/TOGoS/ProjectNotes2/blob/master/2018/StainTest/StainedItems.tef).

This repo may eventually include libraries for parsing.


## Syntax

Basically:

```
[<file-level-header1-key>: <file-level-header1-value>]
[<file-level-header2-key>: <file-level-header2-value>]
...

[<file-level content>]

=[<type>][ <id>]
[<header1-key>: <header1-value>]
[<header2-key>: <header2-value>]
[...]

<content>
```

"`==`" escapes an equal sign at the beginning of a line.
Otherwise, lines beginning with "`=`" start a new entry
A sequence of non-whitespace characters immediately following the "`=`", e.g. "`log`" in "`=log`",
indicates the type of object being declared.
Following that, there may be whitespace, and then more characters.
Everything up to the next newline is the `tef:id-string`.

Following the beginning of an entry is the header block,
terminated by a blank line, end of file, or the start of a new entry.
The header block sonsists of series of headers of
the format "`<key>: <value>`" and/or comment lines, which start with "`# `" or "`#!`".
The space after the colon is required, as keys may contain colons.
Header lines that start with whitespace are considered extensions
to the previous line, following the
'long header fields' rules as described in [RFC822](https://tools.ietf.org/html/rfc822#section-3.1.1)
(i.e. lines are concatenated, minus the LF or CRLF characters between them).

Following the blank line following the header block is the content of the entry
(conceptually, the ```tef:content``` attribute of the entry).
Text is verbatim and includes the newline before the next "`=`", if any.
Lines starting with "`=`" can be escaped by prefixing with another "`=`",
as mentioend earlier.

A simple parser might not give any further meaning to keys.
But for purposes of automatic conversion to RDF,
let's give names structure.

Characters reserved for special meaning in keys are
"`:`" (for namespacing), "`.`" (for attributes), and "`/`" (for components).

Reserved for future extensions:
- Anything starting with "`#`" not followed by whitespace (space, tab, newline)
  or "`!`" (which is allowed for shebang line compatibility)
  - For SGTA or C-preprocessor compatibility?
- Anything starting with "`=?`"
  - For other directives, such as "ignore the previous newline"
- Any key components starting with "`tef:`"

### Sub-attributes and sub-components

We might also want to indicate attributes of attributes,
as well as 'components'.

For these, let's use a dot and slash, respectively:

```
=person Joe
name: Joe
name.length: 3
name/2: e

```

What exactly constitues a sub-attribute vs a sub-component is up to the application,
but for the sake of the above example, let's say
a component of a string is a character.

Theoretical aside: What's the difference between an attribute and a component?
Your height is an attribute, while your heart is a component.
For computer data structures the distinction is fuzzy because
we often mix data and metadata, but think of collection types:
arrays and maps.
An array has a length; that's an attribute.
It also contains objects; those are components.
Some languages, e.g. JavaScript and Lua, do not make this distinction.
I think that is a mistake.
What if you are mapping dictionary words to their definitions,
and one of the words is "prototype"?  Things get confusing.

### Namespacing of keys

A TEF entry is assumed to represent some abstract object
with attributes.
The meaning of the content of the entry is not defined by the TEF syntax;
it might be notes about the object, or, if the object represents a journal entry,
it might be literally the object's content.
So how should a TEF parser pass on that information?
And how can we indicate metadata about the content?

Current proposal: Treat prefixes of the format ```<identifier>:"```
as similar to XML namespace prefixes.
A simple parser can just pass them through,
but one that wants to extract meaning can use the prefix to help it map the key to a 'long name'
(i.e. an RDF predicate name).

The prefix "tef:" is reserved for syntax-level information
(and maps roughly to the "http://ns.nuke24.net/TEF/" xmlns).
i.e. pieces such as the type, ID, and content strings,
which the TEF parser by itself can't assign meaning to.

- ```tef:version``` :: Version of TEF that was followed for this file, optionally indicated as a file-level header
- ```tef:type-string``` :: The string (if any) immediately following the "="
- ```tef:id-string``` :: The string (if any) immediately following the space after the type string
- ```tef:content``` :: The foll content of the entry *after decoding*.
- ```tef:content-type``` :: MIME type of the content
- ```tef:content-length``` :: Length of content, in bytes.  Note that, unlike the HTTP 'content-length' header,
  this indicates the length of the content that has been encoded
  (by applying whatever content-encoding and escaping newline+"`=`" sequences),
  not the number of bytes in the TEF stream that represent the content.
  Indicating content length can resolve ambiguity when the conceptual content does not end with a newline,
  but yet a newline must appear in the TEF stream before the next entry line.
- ```tef:content-encoding``` :: No encodings defined, but I'm reserving this header
  in case I ever need a way to encode binary data.
  Any encoding specified is in addition to "`=`" escaping, which is always implied.
- ```tef:comment``` :: A syntactic comment about this entry

```tef:content-type```, and ```tef:content-encoding```,
while conceptually subattributes of the ```tef:content``` attribute value,
are given these shorthand names, as opposed to e.g. ```tef:content.tef:mime-type```
because this is a very common case, and to keep things looking simple
for the simple parsers who treat the entire key as an atomic string.

Namespacing applies to each component of a key, not to the entire key.


## RDF Namespace

Entry attributes:

- ```http://ns.nuke24.net/TEF/entryScope``` :: entry type based on position in TEF file
  - "`item`" for "`=`"-started entries,
    "`file-header`" for "`=`"-less entries at the top of a TEF file,
    "`file`" for TEF entries that are represented by an entire file (i.e. "=" lines are not parsed)
- ```http://ns.nuke24.net/TEF/typeString``` :: type string of an entry
- ```http://ns.nuke24.net/TEF/idString``` :: ID string of an entry
- ```http://ns.nuke24.net/TEF/content``` :: body text of an entry
  - ```content-type```, ```content-length```, amd ```content-encoding```
    don't really need mappings, as ```content-type``` actually specifies
    ```http://purl.org/dc/terms/format``` on the content itself, and ```content-encoding``` should be handled by the parser
    (the content should be decoded before passed back to the application).
    But for the sake of allowing parsers to represent what they've parsed without having to know those rules,
    ```http://ns.nuke24.net/TEF/contentType```,
    ```http://ns.nuke24.net/TEF/contentLength```, and
    ```http://ns.nuke24.net/TEF/contentEncoding``` can be used.
- ```http://ns.nuke24.net/TEF/comment``` :: arbitrary comment about this entry
- ```http://ns.nuke24.net/TEF/sourceLocation``` :: in what file and at what position within that file
  was this entry found?
  Line/column number should indicate the position of the "=" at the beginning of the entry,
  or the beginning of the file (which is, dumbly but conventionally, (1,1))
  if the entry is of "file" or "file-header" scope.
  Conceptual value is of the type `http://ns.nuke24.net/TOGVM/SourceLocation`.

Note that even though TOGVM defines `http://ns.nuke24.net/TOGVM/sourceLocation`
as a general source location attribute,
TEF defines `http://ns.nuke24.net/TEF/sourceLocation` to help avoid ambiguity
in case objects represented already have source locations.

Types for representing TEF syntactic elements,
in case you want to want to represent them as RDF
(or, more likely, just want some guidance for naming things in your parser code):

- ```http://ns.nuke24.net/TEF/EntryOpener``` :: a "```=<type> <id...>```" line.
  Attributes:
  - ```http://ns.nuke24.net/TEF/typeString```
  - ```http://ns.nuke24.net/TEF/idString```
  - ```http://ns.nuke24.net/TEF/sourceLocation``` - where did this come from?
- ```http://ns.nuke24.net/TEF/Comment``` :: a comment line.
  Attributes:
  - ```http://ns.nuke24.net/TEF/content``` - The stuff after the "#", as a string or byte array.
  - ```http://ns.nuke24.net/TEF/sourceLocation``` - where did this come from?
- ```http://ns.nuke24.net/TEF/Header``` :: a "```<key>: <value>```" line at the beginning of an entry or file.
  Attributes:
  - ```http://ns.nuke24.net/TEF/keyString``` - raw key string
  - ```http://ns.nuke24.net/TEF/valueString``` - raw value string
  - ```http://ns.nuke24.net/TEF/sourceLocation``` - where did this come from?
- ```http://ns.nuke24.net/TEF/ContentChunk``` :: a chunk of file or entry content.
  Attributes:
  - ```http://ns.nuke24.net/TEF/content``` - string or byte array
  - ```http://ns.nuke24.net/TEF/sourceLocation``` - where did this come from?
- ```http://ns.nuke24.net/TOGVM/SourceLocation``` :: a location in a file (defined by [TOGVM-Spec/RDF-VOCAB.tef](https://github.com/TOGoS/TOGVM-Spec/blob/master/RDF-VOCAB.tef))
  - ```http://ns.nuke24.net/TOGVM/SourceLocation/file```
  - ```http://ns.nuke24.net/TOGVM/SourceLocation/lineNumber```
  - ```http://ns.nuke24.net/TOGVM/SourceLocation/columnNumber```
  - ```http://ns.nuke24.net/TOGVM/SourceLocation/lineNumber```
  - ```http://ns.nuke24.net/TOGVM/SourceLocation/columnNumber```

## History

### 0.2.0

- Allow escaping of "`=`" with another "`=`"
- Define `tef:content-length`
- Make up a version number
