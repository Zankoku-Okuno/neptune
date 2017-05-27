Neptune
=======

The most RESTful framework in existence.
Written in Haskell for guaranteed quality.
Oh, and it's concise, lightweight and flexible to boot.

TODO small example site

Read our full documentation on [Viewdocs](http://Zankoku-Okuno.viewdocs.io/neptune/).

Motivation
----------

The more I work with web applications, the more I notice that the current web frameworks just don't cut it.
Their modules are tightly coupled,
or their syntax is verbose and/or magical,
or their utilities are too general to be applicable,
or they fail to deliver on sweeping promises,
and all of them fail to respect REST constraints at some point.

Neptune strives to deliver more. In the core, we support:

- [x] Protocol-independence.
- [x] Uniform interface built on URLs and verbs.
- [x] Content negotiation.
- [ ] Language negotiation.
- [ ] Application state updates.
- [ ] Network caching.
- [ ] URL reversing.

The core is media-type agnostic, so we can't do much there to help you satisfy the hypermedia constraint in your application, or use code-on-demand.
On the other hand, some media types are very common, and should deserve our attention. We support both hypermedia and code-on-demand in:

- [ ] HTML
- [ ] JSON

With these features in place, the "it's too hard" excuse is simply no longer valid.
It is easy to set up a simple application with Neptune, and easy to make it grow to whatever size needed.

Architecture
------------

First off, Neptune has its own request/response data formats.
To hook Neptune up to the web, you'll need to convert between Neptune and whatever network protocol.
Don't worry, we've already written a way to turn a Neptune application into a Wai application.

Within Neptune, there are three major stages of processing: route, action, and format.
Routing, in addition to selecting an action, helps build up parameters to be passed to the resource (action + format).
In the action, the user should obtain the resource data and perform any analysis or shuffling.
Between action and format, there's a content negotiation step which selects out one particular format from among many.
Finally, the format stage creates the body of the output.
Well, that's the rough idea.

In fact, multiple alternate exits from the pipeline are available, such as redirects or authorization failures.
The action is also responsible for updating application state and managing caching policy.

Building a Neptune application follows simple functional composition.
There are no specialized monads or typeclasses to grok before you get to work.

In this description, there has been no mention of HTML, databases, JSON, file systems, or what-have-you.
That is by design.
You plug in your own database (if you need it), your own templates (if you need them), your own whatever.
Neptune is focused on REST, not on how resources are stored, not on the syntax of any particular medium.
This allows Neptune to be applicable to any resources or representations you need.
