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

class Resource:
    def go(self, context, **params):
        try:
            action = self.__getattribute__(context.request.method.upper())
        except AttributeError:
            raise Http405(context)
        else:
            return action(context, **params)

class Renderer:
    def go(self, context, **data):
        from chokma.config import config
        ct = self._negotiate(context) #FIXME return ct and method here
        context.set_header('Content-Type', ct)
        return self.__getattribute__(config.CONTENT_TYPES[ct])(context, **data)

    #FIXME probably not RFC compilant
    def _negotiate(self, context):
        for client in context.request.accept:
            for server in self.targets:
                if client == ('*', '*'):
                    try:
                        return self.default_content_type
                    except AttributeError:
                        pass # continue searching
                elif client[1] == '*' and client[0] == server[0]:
                    return '/'.join(server)
                elif client == server:
                    return '/'.join(server)
        else:
            raise Http406(context)

    @property
    def targets(self):
        from chokma.config import config
        if hasattr(self, '_targets') and self._targets is not None:
            return self._targets
        acc = []
        for ct, method in config.CONTENT_TYPES.items():
            if hasattr(self, method):
                acc.append(tuple(ct.split('/')))
        acc = tuple(acc)
        self._targets = acc
        return acc
