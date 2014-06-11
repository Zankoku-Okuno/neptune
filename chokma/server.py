import sys, os, traceback

from chokma.http.context import Context, Response
from chokma.errors import HttpError, RouteMismatch

from chokma.config import config
from importlib import import_module
endpoints = import_module(config.ENDPOINTS).endpoints

def chokma_app(environ, start_response):
    try:
        response = _normal_response(environ)
        length, body = 0, []
        for x in response.body:
            length += len(x)
            body.append(x)
        response.set_header('Content-Length', str(length))
        response.body = body
    
    except HttpError as exn:
        response = _http_error(exn)
    except Exception as exn:
        traceback.print_exc(file=sys.stderr)
        response = _exn_response(exn)
    
    start_response(response.status, response.headers)
    return response.body


def _normal_response(environ):
    context = Context(environ)
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