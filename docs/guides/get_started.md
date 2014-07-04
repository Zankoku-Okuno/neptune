GetStarted
==========

This guide will help you set up a workspace, install Neptune, and build your first web application with Neptune.

If you're already set up with [Haskell](https://www.haskell.org/platform/), [Cabal](http://www.haskell.org/cabal/) and a sandbox tool like [HsEnv](https://github.com/tmhedberg/hsenv), then you can start from [Installation](#Installation). If not, read the [Set Up](#Set-up) section.

We'll also assume you're using GHC, since that's the most common compiler. If you are using a diferent Haskell compiler and are experienceing problems, please [let us know](https://github.com/Zankoku-Okuno/neptune/issues).

Set Up
======

If you don't already have Haskell installed, go ahead and install the [Haskell Platform](https://www.haskell.org/platform/) now. If you're using a package manager, there may also be a haskell-platform package for your system.

Next, we'll install a Haskell package manager and development sandbox tool.

Cabal
-----

If you installed the Haskell Platform, then you already have Cabal. If not, then you'll have to install it from [source](http://www.haskell.org/cabal/download.html).

If you already have cabal, you can also make sure you have the latest version with:

    $ cabal update
    $ cabal install cabal cabal-install 

HsEnv
-----

If you've used virtualenv in Python, hsenv is the same sort of thing. Now that you have cabal, you can install hsenv easily:

    $ cabal install hsenv

Create a Workspace
------------------

If you're using version control, go ahead and create a repository and enter into it. If no, just create a directory and go inside.

First, create a cabal project:

    $ cabal init

This command will ask you a few questions and set up a project for you. When it asks "Library or Executable?", choose Executable.

Once that's done, edit the `<package-name>.cabal` file. We'll need to edit a couple of fields. For now, find the `main-is` field near the bottom. Uncomment it and set it to `main.hs`

While we're at it, you may as well create a main file and check that everything works:

    $ echo 'main = putStrLn "Goodbyte, cruel world!"' > main.hs
    $ cabal install
    $ <package-name>

With hsenv in place, and with default cabal settings, once the project builds, your executable will be installed, and will even be on the path, so you can run the executable easily. If you're unsure of the name of your executable, it is set in your cabal file, on a line that reads `executable <package-name>`.

Install Neptune
===============

If you're using a cabal package, add the `neptune` package to your `build-depends`. If not, simply `cabal install neptune`.

If you're new to cabal, the `build-depends` field is in the `<package-name>.cabal` file near the `main-is` field. Adding `neptune` to this field sets the `neptune` package (any version) as a dependency of your project. When you `cabal install` in the project directory, all the dependencies will automatically be installed.

First Application
=================

Put the following code in your main file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune

main :: IO ()
main = quickNeptune $ do
    endpoint "home" "GET" "" $ do
    	format $ do
    		medium "text/plain" $ do
    			text "Goodbyte, cruel world!"
```

You can run that now, and see exactly what you'd expect, but let's go through the code anyway.

Explanation
-----------

Before we start, take note of the `OverloadedStrings` extension we've enabled. This can also be enables using the `extensions` executable option in your `.cabal` file.

First, `quickNeptune` runs a development web server over HTTP on port 8080. There are of course other ways to run a web server, but this is good enough for development, and we strongly recommend not building a web-facing server until you understand the security implications of the framework. Anyway, the bulk of interesting code is in that monad after `quickNeptune`.

Next up is the call to `endpoint`. This is the fundamental unit of server-side computation in Neptune. The arguments ot endpoint are (in order)

* A name. It's just simple `Text`, but it will come in handy for url reversing, and once we move on to larger projects, there are some conventions to take account of.
* A verb. In HTTP, these are called methods. Verbs are also simple `Text`, since we don't want to contrain your application's vocabulary unnecessarily.
* A route. This specifies how a request for a resource should be matched as well as how the URL for this resource should be reversed. By using `OverloadedStrings`, many routes become easy to type up. In this case, the empty string means the zero route: this resource is accessible from `localhost:8080/`. We'll see more interesting routes soon enough.
* An action. This is a monad, and we've gone ahead and placed it inside do-notation. Most actions will be more complex than this one, such as retrieving or writing data with a database, checking permissions, kicking off tasks, performing analytics, or what-have-you.

Inside the action, there's a call to `format`. This is seen at the end of most actions, and really doesn't have much of a point except at the end there. The `format` function performs content negotiation, and to do that, it uses a MediaType => Format map, which we very handily build with a monad.

In this case, we only send out a plaintext representation, so we have just one call to `medium`, passing forst the media type, then the format monad.

In this case, the format is simple static text, so we use the `text` function, which simply Utf-8 encodes the given `Text` as the response body.

Run the Code
------------

To run the web server, do
    $ cabal install
    $ <package-name>

Now, you can navigate to `localhost:8080` in a web browser or in a command-line tool like [cURL](http://curl.haxx.se/) or [httpie](http://httpie.org).

Play Around
===========

Routing
-------

TODO: more endpoints, fancier routes

Content Negotiation
-------------------

Verbs
-----

URL Reversing
-------------

TODO
====
assume a package name for ease
