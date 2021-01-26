# TOGoS's Entry Format (TEF)

A file format designed to store a series of objects in flexible and human and machine-readable way,
inspired by Perl POD and MIME.

Useful when you want to record facts, thoughts, logs, or conceptual objects
in a linear way for later processing.

An example:

```
=item WSITEM-2016
# Note that a property can have multiple values, similar to RDF.
# Contrast with e.g. JSON.
# Also note that comments are allowed in the header block,
# and will be ignored by the parser.
coat: Minwax Sedona Red
coat: Minwax Fast-Drying Polyurethane, Clear Satin (applied with brush)
coat: Minwax Wipe-On Poly, Clear Gloss (applied with a cloth)

I coated this item with some stuff!

=log 2019-12-11
current-music: Deadmau5 - SATRN

Super pumped about work today!

=cat-measurements 2019-12-11
length: 27 inches
weight: 6.3 lbs

She took all her pills today.
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
=[<type>][ <id>]
[<header1-key>: <header1-value>]
[<header2-key>: <header2-value>]
[...]

<content>
```

Lines beginning with "`=`" start a new entry.
A sequence of non-whitespace characters immediately following the "`=`", e.g. "`log`" in "`=log`",
indicates the type of object being declared.
Following that, there may be whitespace, and more characters.
This part is commonly used as an identifier.

Following the beginning of an entry is the header block,
terminated by a blank line, end of file, or the start of a new entry.
The header block sonsists of series of headers of
the format "`<key>: <value>`" and/or comment lines, which start with "`#`".
The space after the colon is required, as keys may contain colons.
Header lines that start with whitespace are considered extensions
to the previous line, following the
'long header fields' rules as described in [RFC822](https://tools.ietf.org/html/rfc822#section-3.1.1)
(i.e. lines are concatenated, minus the LF or CRLF characters between them).

Following the blank line following the header block is the content of the entry
(conceptually, the ```tef:content``` attribute of the entry).
Text is verbatim and includes the newline before the next "`=`", if any.
There is no escaping mechanism.

A simple parser might not give any further meaning to keys.
But for purposes of automatic conversion to RDF,
let's give names structure.

Characters reserved for special meaning in keys are
"`:`" (for namespacing), "`.`" (for attributes), and "`/`" (for components).

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

The prefix "tef:" is reserved for syntax-level information.
i.e. pieces such as the type, ID, and content strings,
which that the TEF parser by itself can't assign meaning to.

- ```tef:type-string``` :: The string (if any) immediately following the "="
- ```tef:id-string``` :: The string (if any) immediately following the space after the type string
- ```tef:content``` :: The foll content of the entry; no encoding is assumed.
- ```tef:content-type``` :: MIME type of the content
- ```tef:content-encoding``` :: No encodings defined, but I'm reserving this header
  in case I ever need a way to encode binary data or lines that start with "=".
- ```tef:comment``` :: A syntactic comment about this entry

```tef:content-type```, and ```tef:content-encoding```,
while conceptually subattributes of the ```tef:content``` attribute value,
are given these shorthand names, as opposed to e.g. ```tef:content.tef:mime-type```
because this is a very common case, and to keep things looking simple
for the simple parsers who treat the entire key as an atomic string.

Namespacing applies to each component of a key, not to the entire key.
