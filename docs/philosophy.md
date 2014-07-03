The Philosophy of Neptune
=========================

Most web frameworks develop first by helping to solve some particular task, like shoving out an storefront to a browser (sometimes even a specific browser implementation). These frameworks begin with the concrete and try to climb their way up to abstraction. I find this very interesting, considering the abstract view has already been developed, and apparently everyone knows about it: REST. REST is so abstract as to be difficult to grasp. Wouldn't a smart developer rather work with gravity than against it: start in the clouds of abstraction, and move on down to the concrete?

As a matter of fact, I have yet to find a web framework that cannot be made more RESTful. It's not because REST is magic; it's because the framework developers are working backwards. In turn, the problems in these so-called REST frameworks leads younger developers to misunderstand REST, and the problem only grows worse.

Neptune tackles the REST problem the right way. We begin from a deep understanding of the REST architectual style, both its goals and consequences for an ever-changing ecosystem of heterogenous compute nodes. Only at this point do we build the framework. Only after this REST core is formed do we consider the protocol- and platform-dependent utilities the application developer needs to make something useful.

The overall goal is to create a framework that a) is easy-to-learn, b) makes it quick to build small applications and easy to evolve large ones, c) makes writing RESTful code the path of least resistance, d) is flexible in choice and evolution of both frontends and backends and allows for their graceful evolution, and e) illuminates the real requirements and benefits of the REST architectural style.


Resilience under changes to protocol, data sources, media formats (mimetype, language, &c), application structure, computing stratum

Protocol Independence
---------------------
Every web framework I know is strongly tied to HTTP. PHP is inextricably tied to HTTP, and even HTML. In Python, Pyramid, Bottle and Django all run over WSGI, which just wraps HTTP. Ruby on Rails, Rack and Sinatra all run HTTP servers. Even Haskell's ecosystem, including Yesod and Happstack, all runs over Wai, which again wraps HTTP.

If it's not protocol-independent, it's not RESTful. I can therefore confidently say that no major web framework is RESTful, no matter what it might claim.

Neptune is protocol-independent. When you write a Neptune application, you have a Neptune.Request and you build a Neptune.Response. There is no mention of HTTP anywhere in the core code. Of course, this has consequences for deployment.

When you want to deploy over HTTP (or HTTPS), which admittedly is very often, you have to turn the HTTP request into a Neptune request, then turn the resulting Neptune response into an HTTP response. In fact, We have already coded these translations for you, so when you set up a HTTP server with Neptune, you just call a function. When you want to serve over some other protocol, say sftp, git, or your own custom protocol, you don't need to change any of your application code. Imagine trying that in another framework.

Responses to Criticisms
=======================

Conceptual Overhead
-------------------
Maybe you think there is too much. Then again, I suppose you'd prefer goto to subroutines, or subroutines to closures, or error-codes to exceptions.

The thing REST gives you is a way to easily integrate yourself with a single, vast, decentralized computer: the web. If you want to be in the web, you need a uniform interface, self-describing resources, hypermedia and content negotiation. There's no way around it.

On the other hand, if you want to do something simple (serve only static html), then it's trivial to write a quick function to only allow the GET verb, the text/html mimetype, the en language, and so on. Then, when you want more, the underlying REST plumbing makes it easy to expand.

If you don't want to integrate with other systems, be scalable to many clients, to evolve without hinderance, then go ahead and use something with "less" conceptual overhead. As long remember: you get only what you pay for.

Chatty APIs
-----------

TODO

Use caching. Create appropriate views as resources.

UnCommon Web
------------

The main gripe against REST by these guys is that some applications are desktop-style stuff, with complex "GUI" state (read: application state) management. There are all kinds of overhead managing this GUI state on the server (data marshalling, security, state coherency).

To this, REST responds: code-on-demand. For these complex tasks (like the many steps of a payment process), deliver code (esp. Javascript) to the browser. Best of both worlds: write your complex GUI in one place, leave application state on the client. And in case you're worried about not being able to write in your favorite language, there are compilers for that.
