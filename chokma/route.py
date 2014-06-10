from chokma.errors import Http404

class Route:
    def __init__(self, *segments, endpoint=None):
        self._segs = tuple(segments)
        self._end = endpoint
    
    def go(self, context):
        request = context.request
        path = request.path.copy()
        params, augment = {}, {}
        for seg in self._segs:
            path = seg.test(context, path, params, augment)
        if path:
            raise Http404(request)
        else:
            for attr, value in augment.items():
                context.__setattr__(attr, value)
            if self._end is None:
                raise Exception("TODO bad config")
            return self._end.go(context, **params)

    def append(self, segment):
        if isinstance(segment, tuple):
            segs = self._segs + segment
        else:
            segs = self._segs + (segment,)
        return Route(segs, self._end)
    
    def prepend(self, segment):
        if isinstance(segment, tuple):
            segs = segment + self._segs
        else:
            segs = (segment,) + self._segs
        return Route(segs, self._end)

    def endpoint(self, endpoint):
        return Route(self._segs, endpoint)


class RouteSegment:
    def test(self, context, path, params, augment):
        raise NotImplementedException("%s.test()" % self.__class__.__name__)

    def reverse(self, context, params):
        raise NotImplementedException("%s.reverse()" % self.__class__.__name__)

class Do(RouteSegment):
    def __init__(self, f):
        self._f = f
    
    def test(self, context, path, params, augment):
        self._f(context, params, augment)

    def reverse(self):
        return None

class Literal(RouteSegment):
    def __init__(self, test):
        self._test = test

    def test(self, context, path, params, augment):
        if path and path[0] == self._test:
            return path[1:]
        else:
            raise Http404(context.request)
    
    def reverse(self, context, params):
        return [self._test]

class ParamSegment(RouteSegment):
    def __init__(self, param_name):
        self._name = param_name

    def test(self, context, path, params, augment):
        if not path:
            raise Http404(context.request)
        params[self._name] = self.parse_argument(context, path[0])
        return path[1:]

    def reverse(self, context, params):
        if self._name not in params:
            raise Exception("TODO missing argument when reversing path")
        return self.render_argument(context, params[self._name])

class Slug(ParamSegment):
    def parse_argument(self, context, arg):
        return arg
    def render_argument(self, context, arg):
        return arg

class Natural(ParamSegment):
    def parse_argument(self, context, arg):
        for c in arg:
            if not ('0' <= c <= '9'):
                raise Http404(context.request)
        else:
            return int(arg)
    def render_argument(self, context, arg):
        return str(arg)