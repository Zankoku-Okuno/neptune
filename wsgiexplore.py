#! /usr/bin/env python3
import sys, os
from os import path
os.environ['CHOKMA_CONFIG'] = path.join(path.dirname(path.abspath(__file__)), 'test/config.py')

from chokma.core.context import Context
from chokma.core.route import *
from chokma.core.resource import *
from chokma.core.render import *


class Hello(Resource):
    def GET(self, context):
        return {}
class End(Resource):
    def GET(self, context, **params):
        return {'params': params}

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

def add5(ccontext, params, augment):
    augment['add'] = 5

routes = (
    Route('hi', Hello(), PrintHi(),
        Literal('foo'), Literal('bar')),
    Route('foo', End(), PrintInfo(),
        Literal('foo'), Slug('bar'), Do(add5)),
)


def application(environ, start_response):
    context = Context(environ)
    for route in routes:
        try:
            endpoint, params = route.go(context)
        except Http404:
            continue
        endpoint.resource.go(context, **params)
        endpoint.renderer.go(context)
        response = context.response
        status = '200 OK'
        response_headers = response._headers
        body = list(response._body)
        break
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

