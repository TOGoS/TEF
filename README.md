# TOGoS's Entry Format (TEF)

A file format designed to store a series of objects in flexible and human and machine-readable way,
inspired by Perl POD and MIME.

Useful when you want to record facts, thoughts, logs, or conceptual objects
in a linear way for later processing.

An example:

```
=item WSITEM-2016
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

I've been using this format for over a decade

See [stuff.tef](./stuff.tef) for a longer example.
Or [music.txt](http://www.nuke24.net/music/music.txt),
which is used to generate [the music page on my website](http://www.nuke24.net/music/).
My [project log entries](http://www.nuke24.net/plog/entries/) are also written
in this format, but with each file being a self-contained entry.

This repo may eventually include libraries for parsing.
