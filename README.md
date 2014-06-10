Chokma
======

A RESTful python web framework: lightweight, extensible, decoupled, secure.

Motivation
==========

I've been working with Django professionally, and have become disillusioned with it. I've also tried Yesod, and the same thing happened. I think the problem is twofold.

First, both frameworks make all kinds of promises, and then fail to live up to some. For Django, it's a lack of simplicity cause by poor documentation and unnecessarily complex metaprogramming. For Yesod, the documentation never really explained the types involved, and the ORM got in the way of running everyday relational queries.

Second, the frameworks have tons of internal coupling. It's difficult to replace parts of the framework you don't like. All the choices have been made for you, and their documented that way, even if you do try to customize anything. Django in particular assumes that if templates a need for complex presentation logic, then it's been built wrong. That ideology forces presentation logic elsewhere, destroying the MVC architecture.


Architecture
============

The important thing a web framework should do is produce a response from a request. A Chokma server manipulates python abstractions of the HTTP request and response. Within the application, a Context object is passed through a processing pipeline. The context includes the request, the response being built and any additional information the developer might need. The pipeline consists of a routing, a resource, and a renderer. In the interests of decoupling, these stages consist only of an interface, though Chokma does provide default implementations. With some work, this framework could be adapted to host non-HTTP servers.

Routing dispatches on the requested schema+URI. During this stage, parameters may be collected to be passed to the resource.
Resources are objects that dispatch to one of their methods based on the request method. They should be responsible for gathering data in python, which will then be passed on to the renderer.
Renderers are objects that dispatch to one of their methods based on the Accept HTTP header. They should be generators producing bytes objects, just as accepted by WSGI.
The important point is that content generation is automatically based on resources, methods, and content negotiation. HATEOAS is already taken care of in many common data formats, so all you the developer has to worry about is to build your machine-readable formats accordingly.

So far, there has been no mention of HTML, databases, JSON, file systems, or what-have-you. That is by design. You plug in your own database (if you need it), your own templates (if you need them), your own whatever. Chokma is focused on gluing together web applications, not how those applications store and retrieve their data. In practice, having a database interface and a HTML generator are useful, and has those libraries as well, but in a form that matches its philosophy.


Projected Features
==================

  * Generate content in response to requests' schema, URL, method, accept content-type, and accept language.
  * Render common data formats.
  * Virtual filesystem.
  * Marshal data between SQL and Python.
  * Generate database queries.
  * Session, cookie, and auth utilities.
  * General template language, and specializations for HTML and CSS.
