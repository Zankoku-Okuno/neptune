
from chokma.http.pipeline import Endpoint, Route, Resource, Renderer
from chokma.errors import Http404
from chokma.http.routing import *
from chokma import fs



class Hello(Resource):
    def GET(self, context):
        return {}
class End(Resource):
    def GET(self, context, **params):
        return {'params': params}
class File(Resource):
    def GET(self, context, filename):
        try:
            filepath = fs.resolve('test', filename)
        except ValueError:
            raise Http404(context)
        context.set_header('Content-Type', 'text/plain')
        raise fs.Sendfile(filepath)

class PrintHi(Renderer):
    def html(self, context):
        yield b"Hello!"
class PrintInfo(Renderer):
    default_content_type = 'text/plain'
    def text(self, context, params):
        yield str(context.add).encode('utf-8')
        yield b"\n"
        yield str(params).encode('utf-8')
        yield b"\n"
        yield str(context.request.accept).encode('utf-8')
    def html(self, context, params):
        from chokma import html
        doc = html.Document()
        doc.title = "wsgiexplore info"
        doc.append(
            html.p(context.add).id('first'),
            html.p(params),
            html.p(context.request.accept),
            )
        yield from html.render(doc)

def add5(ccontext, params, augment):
    augment['add'] = 5

endpoints = (
    Endpoint('hi', Hello(), PrintHi(),
        Route(Literal('foo'), Literal('bar')) ),
    Endpoint('file', File(), None,
        Route(Literal('file'), Slug('filename')) ),
    Endpoint('foo', End(), PrintInfo(),
        Route(Literal('foo'), Slug('bar'), Do(add5)) ),
)