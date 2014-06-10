Chokma for Pipeline Writers
===========================

The Chokma pipeline consists of three phases: routing, resource acquisition, and rendering. Before the pipeline is entered, a Request object is set up with all relevant input information, and an empty Response object is created. Before the pipeline is exited, the Response object should be filled out. Both the request and response are aggregated in a Context object, which may carry additional information at an application developer's discretion. The contents of a complete Request or Response object are dependent on the nature of the server.

Routing
-------

The Route Protocol requires only one method, 'go', which should take a context as its sole arguement. If the route matches the input location, then the context may be modified and the method must return a kwargs dictionary. If the route does not match the input location, then the context must not be modified and an appropriate exception must be raised.

Resources
---------

The Resource protocol requires only one method, 'go', which should take a context as its first argument, but may take any number of additional keyword arguments. It must return a kwargs dictionary.

Rendering
---------

The Renderer protocol requires only one method, 'go', which should take a context as its first argument, but may take any number of additional keyword arguments. It must return a value suitable for completing the response.

Endpoint
--------

An endpoint is a named combination of route, resource and renderer. It should have four attributes
	* [name] a string
	* [route] an object responding to the Route protocol
	* [resource] an object responding to the Resource protocol
	* [renderer] an object responding to the Renderer protocol

The WSGI Pipeline
=================

The request is populated from the WSGI environment. In particular, the attributes 'scheme', 'path', 'method' and 'accept' are set according to the HTTP headers.

Routes test the path according to a sequence of PathSegments. Each path segment may make tests against the path and context, prepare changes to the context and output kwargs, and return a new path to be considered by subsequent segments.

Resource objects uppercase the HTTP method and dispatch to its Python method of the same name.

Renderer objects perform content negotiation to determine which of its Python methods to which it should dispatch.

The response is expected to have its body attribute set with a generator of bytes objects. It may also have headers set.

If at any point there is an unhandled error, an appropriate error response is generated.