from urllib.parse import quote
from chokma.errors import RouteMismatch, Http405, Http406


class Endpoint:
    def __init__(self, name, resource, renderer, route):
        self.name = name
        self.route = route
        self.resource = resource
        self.renderer = renderer

    # TODO prefix/postfix the route with add'l segments 
        
class Route:
    def __init__(self, *segments):
        self._segs = tuple(segments)
    
    def go(self, context):
        request = context.request
        path = request.path.copy()
        params, augment = {}, {}
        for seg in self._segs:
            path = seg.test(context, path, params, augment)
        if path:
            raise RouteMismatch()
        else:
            for attr, value in augment.items():
                context.__setattr__(attr, value)
            return params

    def reverse(self, context, **params):
        acc = []
        for seg in self._segs:
            acc += seg.reverse(context, **params)
        return '/'.join(map(lambda x: quote(x, safe=''), acc))

class Resource:
    def go(self, context, **params):
        try:
            action = self.__getattribute__(context.request.method.upper())
        except AttributeError:
            raise Http405(context)
        else:
            return action(context, **params)

class Renderer:
    def __init__(self):
        # set up render functions to dispatch from a cannonical MIME type to its alias
        from chokma.config import config
        if not hasattr(config, 'CONTENT_TYPES'):
            return
        # use a function creator here, because python is dumb about scope
        # if we just used that lambda in-place, the value of 'name' would change each time through the loop
        def create_alias(name):
            return lambda context, **data: self.__getattribute__(name)(context, **data)
        for ct, alias in config.CONTENT_TYPES.items():
            pyct = _pythonize_mime(ct)
            if not hasattr(self, pyct) and hasattr(self, alias):
                self.__setattr__(pyct, create_alias(alias))

    def go(self, context, **data):
        ct, method = self._negotiate(context)
        context.set_header('Content-Type', ct)
        return self.__getattribute__(method)(context, **data)

    #FIXME probably not RFC compilant
    def _negotiate(self, context):
        for client in context.request.accept:
            if client == ('*', '*'):
                try:
                    return self.default_content_type, _pythonize_mime(self.default_content_type)
                except AttributeError:
                    continue
            elif client[1] == '*':
                pyclient_prefix = _pythonize_mime(client[0]+'/')
                # FIXME I should allow prefered mimetypes, in case the media range can match many methods
                for method in dir(self):
                    if method.startswith(pyclient_prefix):
                        break
                else:
                    continue
                # this heuristic simply lowercases the method, leaving underscores alone
                # FIXME in case this heuristic fails, I need a more exact way to determine the delivered content type
                # an additional heuristic is to see if there is an alias that would pythonize to the found method
                # finally, I might just need to look up in a dictionary somewhere
                server_suffix = method[len(pyclient_prefix):].lower()
                server = client[0] + '/' + server_suffix
                return server, method
            else:
                client = '/'.join(client)
                pyclient = _pythonize_mime(client)
                if hasattr(self, pyclient):
                    return client, pyclient
        else:
            raise Http406(context)

def _pythonize_mime(mime):
    py = ''
    for c in mime.upper():
        py += '_' if c in './-' else c
    return py