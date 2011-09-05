gloss-web -- Web-based front-end to Ben Lippmeier's gloss package

With gloss-web, you can host an online development environment capable of
building applications in the gloss graphics library with Haskell!

--------

WARNING: This application runs user-uploaded code on your server.  It uses the
SafeHaskell extension and only evaluates pure values, so it ought to be safe.
However, SafeHaskell was released very recently, so it's up to you to decide
whether you trust it or not.

In addition, there's currently no protection against:

* Eating up 100% of your CPU time.
* Using bandwidth to transfer infinite-sized results.
* Crashing the server by using unbounded amounts of memory.

Because of this, it's probably not appropriate for running on a shared server
with other important applications.

---------------
How To Install:
---------------

First, you'll need a modified version of gloss, in order to use
it with SafeHaskell.

1. cabal unpack gloss
2. cd gloss-*
3. Edit gloss.cabal and add 'Extensions: Trustworthy' to the executable section.
4. cabal install

Next, you need to tell GHC that you trust the 'base' and 'gloss' packages.

5. sudo ghc-pkg trust base
6. ghc-pkg trust gloss

At this point, you'll have a "trustworthy" version of gloss installed, and have
marked base as "trusted".  Note that this has security implications in case a
security vulnerability should be discovered in gloss or base, so technically
speaking you ought to vet at least the gloss source code (base has been
inspected already) to convince yourself it's okay.  We'll wait for you to do
that... okay, welcome back.

Now you will want to build gloss-web-adapters, which is a small package that
plays a role in the interface between gloss-web and user-submitted code.

7.  cd gloss-web-adapters
8.  cabal install
9.  ghc-pkg trust gloss-web-adapters
10. cd ..

Finally, it's time to install gloss-web itself.

11. cabal install

You can now run gloss-web from the current working directory.  You can NOT run
the copy that was installed in ~/.cabal/bin, since the application depends on
a lot of relative paths.

12. ./dist/build/gloss-web/gloss-web

The default port is 8000, so you can now visit 'http://localhost:8000' to
access the server.


How to Use:
-----------

1. Visit the URL for the server (if it's running locally and you didn't pass
   any command line parameters, that's http://localhost:8000)
2. Click the links to run in picture, animation, or simulation mode.
2. Enter your code in the editor you see on that page.
3. Click "Run" to see the resulting image, animation, or simulation.

If you don't see the image load when you click "Run", make sure you enable
JavaScript and disable any popup blockers, and that your browser is supported.
Supported browser versions are Firefox 6.0 or higher, and recent versions of
Chrome, Safari, or Opera.  Other browsers may have limited functionality.
Internet Explorer, for example, works for pictures, but not animations or
simulations.  The same is true for the Android browser, but Firefox 6.0 on
the Android operating system works fine.

Your source code is stored in a cookie in your browser, so you'll be able to
quit, and go back later to finish editing.  That said, though, cookies tend
to get deleted sometimes, so you probably want to keep another copy
elsewhere!  (It's also stored on the server in the tmp directory, but you
can't get it from there through the web interface yet.)


How to Help:
------------

This is a bare skeleton of the project.  It needs a lot of work.  See the
issues list at

    https://github.com/cdsmith/gloss-web/issues

For things that need work.  Your patches and suggestions are always
appreciated!

