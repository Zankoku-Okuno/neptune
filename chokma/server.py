import sys, os, traceback
from os import path

from chokma.http.context import Context, Response, EmptyResponse
from chokma import http
from chokma import fs
from chokma.errors import HttpError, Http404, RouteMismatch

from chokma.config import config
from importlib import import_module
endpoints = import_module(config.ENDPOINTS).endpoints

def chokma_app(environ, start_response):
    try:
        try:
            context = Context(environ)
            response = _normal_response(context)
            length, body = 0, []
            for x in response.body:
                length += len(x)
                body.append(x)
            response.set_header('Content-Length', str(length))
            response.body = body
        
        except EmptyResponse as exn:
            response = _empty_response(context, exn.code)
        except fs.Sendfile as exn:
            response = _file_response(context, exn.filepath)
    except HttpError as exn:
        response = _http_error(exn)
    except Exception as exn:
        traceback.print_exc(file=sys.stderr)
        response = _exn_response(exn)
    
    start_response(http.format_code(response.code), response.headers)
    return response.body


def _normal_response(context):
    for endpoint in endpoints:
        try:
            params = endpoint.route.go(context)
            data = endpoint.resource.go(context, **params)
            if data is None:
                data = dict()
            body = endpoint.renderer.go(context, **data)
            if body is None:
                body = [b'']
            context.response.body = body
            return context.response
        except RouteMismatch:
            continue
    else:
        raise Http404(context)

def _empty_response(context, code):
    response = context.response
    response.code = code
    response.set_header('Content-Length', '0')
    response.body = [b'']
    return response

def _file_response(context, filepath):
    if not path.isfile(filepath):
        raise Http404(context)
    
    response = context.response
    response.code = 200
    if not context.has_header('Content-Type'):
        pass #STUB guess content type, or else application/octet-stream
    stat = os.stat(filepath)
    context.set_header('Content-Length', str(stat.st_size))
    chunk_size = 1024 #TODO make configurable
    response.body = fs.file_chunks(filepath, chunk_size=chunk_size)
    return response

def _http_error(exn):
    response = Response(exn.code)
    response.set_header('Content-Type', 'text/plain')
    response.body = [http.format_code(exn.code, no_number=True).encode('utf-8')]
    response.set_header('Content-Length', str(sum(map(len, response.body))))
    return response

def _exn_response(exn):
    response = Response(500)
    response.set_header('Content-Type', 'text/plain')
    response.body = [b"Internal server error."]
    response.set_header('Content-Length', str(sum(map(len, response.body))))
    return response