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
