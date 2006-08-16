;; -*- mode: outline; -*-

CL-STRINGS
----------

This stuff isn't really ready yet because there's no documentation and
the tests are incomplete, and there's no proper way to make an ASDF
installable package, etc, etc, etc.


* Motivation

I'm not sure there's really any need for a string convenience library
for Common Lisp, since so much of what you might want to do with
strings is available with the usual list or sequence functions already
provided.  What's not available with those is neatly filled by Edi
Weitz's CL-PPCRE.

Still, just for fun, I wanted to write a library which mimicked the
java String class, with a little StringTokenizer thrown in for good
measure.  I might take a look at the C# string type and fit a few more
things in: likewise Python.

I don't think this stuff is all that Lispy, but it's fun.

* to do

Still stuff to do for a "version 1" release:

 * complete the tests

 * make sure each function has a doc string

 * generate HTML documentation, either automatically from the source,
   or just by hand.

 * create a CLIKI page so folks can:

     (asdf-install:install 'cl-strings)

 * announce on Gardeners and invite folks to make other such imitative
   libraries for fun and profit --- well, fun, anyway.

