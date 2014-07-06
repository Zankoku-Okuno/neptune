Deep Glossary
=============

Negotiation
-----------

Negotiation is actually a fairly deep subject. Yes, both the client and server are involved, but so is the question of read vs. write.

Consider this scenario:
	
	* A client produces a request body.
	* The server interprets the request body
	* The server produces a response body
	* The client interprets the response body.

At least three negotiation steps occur here. Usually, negotiation is not required when a client creates a request body, but if code-on-demand gets involved, the client code might need to determine exactly what the client is capable of.

The server can only understand so many media types, so when the server interprets the request body, it looks at the `Content-Type` request header. When that content-type is supported, then all is good. But if not, then the server should respond with an appropriate error code and a list of what kinds of request bodies are supported.
In HTTP, that error code is `415 Usupported Media Type`.
This can happen when, for example, a server is only set up to interpret JSON request bodies, but a client sends a urlencoded form.

The server can only generate so many media types, and the client can only interpret so many. Thus, the client send s along an `Accept` header in the request, which the server then uses to select one of its representations. If the set of media types understood by the client is disjoint with the set the server can produce, then there's a problem, and the server should respond with an appropriate error code and a list of what kinds of response bodies it can produce.
In HTTP, that error code is `406 Not Acceptable`.
This can happen when, for example, if a client can only understand YAML, but the server can only generate JSON and HTML.

Finally, when the client needs to interpret the response body, it looks at the `Content-Type` response header. If the client was built with care, then there shouldn't be any problem at this point, and the client can just select an appropriate action based on the result, such as render to the screen, run in a JavaScript interpreter, or download to storage.

This discussion has so far only treated media type negotiation. Other forms of negotiation also exist, and follow roughly the same pattern. For example, Neptune uses language negotiation to help with internatinoalization, and SSL uses negotiation during the handshake to select an encryption algorithm.