#! /usr/bin/env python3
import sys, os
os.environ['CHOKMA_CONFIG'] = '/home/edemko/Documents/chokma/test/config.py'

from chokma.context import Context
from chokma.route import *
from chokma.resource import *
from chokma.render import *

class PrintHi(Renderer):
    def html(self, context):
        yield b"Hello!"
class PrintInfo(Renderer):
    default_content_type = 'text/plain'
    def text(self, context):
        yield str(context.add).encode('utf-8')
        yield b"\n"
        yield str(context.params).encode('utf-8')
        yield b"\n"
        yield str(context.request.accept).encode('utf-8')
    def html(self, context):
        from chokma import html
        doc = html.Document()
        doc.title = "wsgiexplore info"
        doc.append(
            html.p(context.add).id('first'),
            html.p(context.params),
            html.p(context.request.accept),
            )
        yield from html.render(doc)

class Hello(Resource):
    _renderer = PrintHi()
    def GET(self, context):
        return {}
class End(Resource):
    _renderer = PrintInfo()
    def GET(self, context, **params):
        return {'params': params}



def add5(ccontext, params, augment):
    augment['add'] = 5
routes = (
    Route(Literal('foo'), Literal('bar'), endpoint=Hello('hi')),
    Route(Literal('foo'), Slug('bar'), Do(add5), endpoint=End('foo')),
)



def application(environ, start_response):
    context = Context(environ)
    for route in routes:
        try:
            response = route.go(context).response
            status = '200 OK'
            response_headers = response._headers
            body = list(response._body)
            break
        except Http404:
            pass
    else:
        status = '404 Not Found'
        response_headers = [( 'Content-Type', 'text/plain' )]
        body = ['Resource not found.\n'.encode('utf-8'),
                str(ex.request.path).encode('utf-8')
               ]

    len_acc = 0
    for piece in body:
        len_acc += len(piece)
    response_headers.append(( 'Content-Length', str(len_acc) ))
    start_response(status, response_headers)
    return body

from wsgiref.simple_server import make_server
httpd = make_server('localhost', 8080, application)

try:
    print("Starting server...")
    httpd.serve_forever()
except KeyboardInterrupt:
    print("\nGoodbye!")
    sys.exit(0)

