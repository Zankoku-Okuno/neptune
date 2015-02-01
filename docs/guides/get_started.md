Get Started
===========

This guide will help you set up a workspace, install Neptune, and build your first web application with Neptune.

If you're already set up with [Haskell](https://www.haskell.org/platform/), [Cabal](http://www.haskell.org/cabal/) and a sandbox tool like [HsEnv](https://github.com/tmhedberg/hsenv), then you can start from [Installation](#user-content-install-neptune).
If not, read the [Set Up](#user-content-set-up) section.

We'll also assume you're using GHC, since that's the most common compiler.
If you are using a different Haskell compiler and are experiencing problems, please [let us know](https://github.com/Zankoku-Okuno/neptune/issues).

All of the example Neptune applications here are also available in [the source code](https://github.com/Zankoku-Okuno/neptune/examples). They may be run from within that directory using `runhaskell`, assuming you've installed Neptune.

Set Up
======

If you don't already have Haskell installed, go ahead and install the [Haskell Platform](https://www.haskell.org/platform/) now.
If you're using a package manager like apt, yum or homebrew, there may also be a haskell-platform package for your system.

Next, we'll install a Haskell package manager and development sandbox tool.

Cabal
-----

If you installed the Haskell Platform, then you already have Cabal.
If not, then you'll have to install it from [source](http://www.haskell.org/cabal/download.html).

If you already have cabal, you can also make sure you have the latest version with:

    $ cabal update
    $ cabal install cabal cabal-install 

HsEnv
-----

If you've used virtualenv in Python, hsenv is the same sort of thing.
Now that you have cabal, you can install hsenv easily:

    $ cabal install hsenv

Create a Workspace
------------------

If you're using version control, go ahead and create a repository called `neptune-app` and enter into it.
If not, just create a directory (same name) and go inside.

First, create a cabal project:

    $ cabal init

This command will ask you a few questions and set up a project for you.
When it asks "Library or Executable?", choose Executable.
Otherwise, the defaults should be good enough.
You may need to `touch LICENSE` to get cabal to shut up, depending on which license you opted for.

Next, activate your sandbox.

    $ hsenv init
    $ source .hsenv/bin/activate

Once that's done, edit the `neptune-app.cabal` file.
We'll need to fill in a couple of fields.
For now, find the `main-is` field near the bottom.
Uncomment it and set it to `main.hs`

While we're at it, you may as well create a main file and check that everything works:

    $ echo 'main = putStrLn "Goodbyte, cruel world!"' > main.hs
    $ cabal install
    $ neptune-app

Install Neptune
===============

If you're using a cabal package, add the `neptune` package to your `build-depends`.
If not, simply `cabal install neptune`.

If you're new to cabal, the `build-depends` field is in the `neptune-app.cabal` file near the `main-is` field.
Adding `neptune` to this field sets the `neptune` package (any version) as a dependency of your project.
When you `cabal install` in the project directory, all the dependencies will automatically be installed.


First Application
=================

Put the following code in your main file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai

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

Before we start, take note of the `OverloadedStrings` extension we've enabled.
This can also be enables using the `extensions` executable option in your `.cabal` file.

First, `quickNeptune` runs a development web server over HTTP on port 8080.
There are of course other ways to run a web server, but this is good enough for development, and we strongly recommend not building a web-facing server until you understand the security implications of the framework.
Anyway, the interesting code is in that monad after `quickNeptune`.

Next up is the call to `endpoint`.
This is the fundamental unit of server-side computation in Neptune.
The arguments to endpoint are (in order):

* A name. It's just simple `Text`, but it will come in handy for url reversing. Once we move on to larger projects, there are some conventions to take account of.
* A verb. In HTTP, these are called methods. Verbs are also simple `Text`, since we don't want to contrain your application's vocabulary unnecessarily.
* A route. This specifies how a request for a resource should be matched as well as how the URL for this resource should be reversed. By using `OverloadedStrings`, many routes become easy to type up. In this case, the empty string means the zero route: this resource is accessible from [localhost:8080](localhost:8080). We'll see more interesting routes soon enough.
* An action. This is a monad, and we've gone ahead and placed it inside do-notation. Most actions will be more complex than this one, such as retrieving or writing data with a database, checking permissions, kicking off tasks, performing analytics, and/or what-have-you.

Inside the action, there's a call to `format`.
This is seen at the end of most actions, and really doesn't have much of a point except at the end there.
The `format` function performs content negotiation, and to do that, it uses a MediaType => Format map, which we very handily build with a monad.

In this case, we only send out a plaintext representation, so we have just one call to `medium`, passing first the media type, then the format monad.

In this case, the format is simple static text, so we use the `text` function, which simply Utf-8 encodes the given `Text` as the response body.

Run the Code
------------

To run the web server, do

    $ cabal install
    $ neptune-app

Now, you can navigate to `localhost:8080` in a web browser or in a command-line tool like [cURL](http://curl.haxx.se/) or [httpie](http://httpie.org).

A Quick Tour
============

Here, we'll just briefly go through and examine some of the ways this small application can be expanded.
We won't do too much explanation here; we'll just give examples of a few possibilities and leave you to play around with it.

Content Negotiation
-------------------

My guess is that looking at that `format $ medium "text/plain" $ text` code, you were thinking "why so verbose?".
In fact, I've shown the most verbose form, which also happens to be the most powerful form.
You can always define your own shortcuts, and in many cases, we'd encourage it.
For now, though, plaintext is boring.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = quickNeptune $ do
    endpoint "home" "GET" "" $ do
        format $ do
            medium "text/plain" $ do
                text "Goodbyte, cruel world!"
            medium "text/html" $ do
                htmlFile <- liftIO $ LBS.readFile "home.html"
                lbs htmlFile -- use a lazy ByteString for the response body
```

Create an appropriate html file in `home.html`, and see the results.
If you're using httpie, here are some relevant requests:

    $ http :8080
    $ http :8080 Accept:text/plain
    $ http :8080 Accept:text/html


Routing
-------

Routing is a big subject, but in the interests of brevity, we won't show off much of the power here.
This is certainly enough to get some real work done (just ask [Scotty](https://github.com/scotty-web/scotty), which inspired us).
There's even more power waiting if you're willing to write your own `Route`s.
For more detail, check out the [Master Routes](routes.md) guide.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = quickNeptune $ do
    endpoint "home" "GET" "" $ do
        format $ do
            medium "text/plain" $ do
                text "Goodbyte, cruel world!"
            -- Just showing off how common code can be extracted
            -- The same thing can be done with monads
            msgHtml "Lord, I feel the ocean swayin' me..."
    -- Notice the ":msg" path segment in this route. This captures that
    --    segment as a QDatum
    endpoint "msg" "GET" "echo/:msg" $ do
        -- We use qDatum_ff to get the part of the URL captured by ":msg"
        -- The two 'f's are a double-force:
        --   it must be present or error out (no danger of that here)
        --   it must parse to the correct type (we only need Text, so
        --       again no danger here)
        msg <- qDatum_ff "msg"
        format $ msgHtml msg
    endpoint "static" "GET" "static/..." $ do
        --While I'm at it, here's how to cache. The argument is in seconds.
        setCache $ 24 * 60 * 60
        -- A parameter in the URI path is called a "datum".
        -- The `pathKey` is just a pre-defined way to access datums.
        --     You can easily create your own keys.
        -- Use the `datum` function to retrieve data from this `Vault`.
        path <- T.unpack . T.intercalate "/" <$> datum_f pathKey
        -- The `anyFormat` function ignores the `Accept` header and does not
        --     produce a `Content-Type` header.
        anyFormat $ sendfile ("static/" ++ path)

msgHtml :: Text -> Formats
msgHtml msg = medium "text/html" $ do
    htmlFile <- liftIO $ T.readFile "home.html"
    let (pre, rest) = T.breakOn slot htmlFile
        post        = T.drop (T.length slot) rest
    text $ pre <> msg <> post
    where
    slot = "{{msg}}" :: Text
```

So as not to complicate things, I've gone ahead and done the Html templating wrong.
This app is subject to XSS attacks pretty easily.
In a real application, you'll want to use a templating library that can protect from injection automatically.

    $ http ':8080/echo/<script>alert("Hi!")<%2Fscript>'

For the `static` endpoint, it should be noted that there's no directory-traversal flaw.
Paths are automatically normalized to remove the segments `//`, `/./` and `/../`.
The `/../` segment, when removed, also removes the previous segment, but if there is no previous segment, then the '/../' is simply removed.
You can rest assured that these user-supplied paths will never spill over into parent or sibling directories.

Verbs and More
--------------

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai
import qualified Data.Text.IO as T
import qualified Data.Vault.Lazy as Vault

main :: IO ()
main = quickNeptune $ do
    let fname = "upload/name"
    endpoint "name" "GET" "" $ do
        format $ do
            plaintext $ sendfile fname
            json $ do
                -- You can do plenty of work in the Format monad
                contents <- liftIO $ T.readFile fname
                text $ "{'name':\"" <> contents <> "\"}"
    endpoint "name" "PUT" "" $ do
        -- Extracting the body requires that we understand its Content-Type
        body <- parseBody [("text/plain", return . toStrictT . decodeUtf8L)]
        liftIO $ T.writeFile fname body
        -- Here's a simple url reversal: no datums and no parameters
        goto <- url "name" Vault.empty []
        redirect goto

-- If some media types get reused often, we recommend making shortcuts
plaintext :: Format -> Formats
plaintext = medium "text/plain"

json :: Format -> Formats
json = medium "application/json"
```

* TODO: accept JSON input, better JSON output (like Aeson or something)
* TODO: make sure to send appropriate error responses when getting unsuitable JSON

TODO
====
* review for and note any the security flaws
* cookies