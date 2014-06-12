import sys, os, traceback
from os import path

from chokma.http.context import Context, Response
from chokma.errors import HttpError, RouteMismatch
from chokma.fs import Sendfile

from chokma.config import config
from importlib import import_module
endpoints = import_module(config.ENDPOINTS).endpoints

def chokma_app(environ, start_response):
    try:
        context = Context(environ)
        response = _normal_response(context)
        length, body = 0, []
        for x in response.body:
            length += len(x)
            body.append(x)
        response.set_header('Content-Length', str(length))
        response.body = body
    
    except Sendfile as exn:
        response = _file_response(context, exn.filepath)
    except HttpError as exn:
        response = _http_error(exn)
    except Exception as exn:
        traceback.print_exc(file=sys.stderr)
        response = _exn_response(exn)
    
    start_response(response.status, response.headers)
    return response.body


def _normal_response(context):
    for endpoint in endpoints:
        try:
            params = endpoint.route.go(context)
            data = endpoint.resource.go(context, **params)
            body = endpoint.renderer.go(context, **data)
            context.response.body = body
            context.response.status = '200 OK'
            return context.response
        except RouteMismatch:
            continue
    else:
        raise Http404(context)

def _file_response(context, filepath):
    if not path.isfile(filepath):
        return _http_error(Http404())
    
    response = context.response
    response.status = '200 OK'
    if not context.has_header('Content-Type'):
        pass #STUB guess content type, or else application/octet-stream
    stat = os.stat(filepath)
    context.set_header('Content-Length', str(stat.st_size))
    def body():
        chunk_size = 1024 #TODO make configurable
        with open(filepath, 'rb') as fp:
            while True:
                data = fp.read(chunk_size)
                if not data:
                    break
                yield data
    response.body = body()
    return response

def _http_error(exn):
    response = Response()
    response.status = exn.status
    response.set_header('Content-Type', 'text/plain')
    response.body = [exn.status_str.encode('utf-8')]
    response.set_header('Content-Length', str(sum(map(len, response.body))))
    return response

def _exn_response(exn):
    response = Response()
    response.status = '500 Internal Server Error'
    response.set_header('Content-Type', 'text/plain')
    response.body = [b"Internal server error."]
    response.set_header('Content-Length', str(sum(map(len, response.body))))
    return response