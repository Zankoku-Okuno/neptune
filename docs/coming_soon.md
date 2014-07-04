Coming Soon
===========

Neptune is currently an experiment, and so its API may change significantly in the near future. To help users avoid accidental coupling to any current sub-par subsystems, and potential users understand how Neptune might soon evolve to meet their needs, we provide this list of expected changes that will have a significant impact on the library's quality.

If you can help contribute, fork us on [Github](https://github.com/Zankoku-Okuno/neptune).

URL Manipulation
----------------

Neptune strives to provide rich facilities for generating URLs. Unfortunately, our current model is not standards-compliant. The existing `url` package and `Network.URI` in `network` look suitably rich, but they rely on the inefficient `String` type and so don't integrate wel with Neptune, which uses the more modern `ByteString` and `Text` types.

The other half of the equation is displaying a concise interface to the user. We want to provide the full flexibility of URIs to the application developer, but at the same time make common tasks the shortest. A suitable system may take some work, but we are confident it can be done without too much difficulty.

Internationalization
--------------------

I have been so far unable to find any existing Haskell libraries for negotiating language according to the relevant RFCs. I will likely have to write my own IETF language tag parser. Luckily, this is a relatively self-contained subsystem, so anyone can help contribute to such a parser as long as they understand the RFCs.

A larger task looms about how to internationalize individual pieces of text. In particular, I need to decide between one-time language negotiation and per-string negotiation. While per-string brings additional flexibility, one-time is much more efficient. Ideally, we can figure out a scheme that provides real flexibility under a negotiate-once system.

Unicode Normalization
---------------------

There's a great Haskell package `text-icu` which provides all sorts of standards-compliant unicode manipulations. Unfortunately, it requires native libraries. It would certainly be easier for the user not to deal with an extra step, but if it comes to it, then we'll do it.

Addons
------

Neptune's open structure gives other developers plenty of opportunity to extend the Neptune framework with tools of their own, such as generating various content formats, encryption, authorization, databases and filesystems, and so on. The Neptune project would like to provide addons for common tasks, but at the moment, we have not yet done so.

Even especially expected addons, like HTML generation, database integration, or serving over SSL just don't exist yet. Of course, it would take only a little work to glue any existing general Haskell implementations to Neptune, but please consult us before releasing any `neptune-*` package. If you do consult us, we can probably support you as an official Neptune Project contributor, and help iron out any kinks that might come up.