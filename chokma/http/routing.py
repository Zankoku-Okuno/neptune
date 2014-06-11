from chokma.errors import RouteMismatch


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
            raise RouteMismatch()
    
    def reverse(self, context, params):
        return [self._test]

class ParamSegment(RouteSegment):
    def __init__(self, param_name):
        self._name = param_name

    def test(self, context, path, params, augment):
        if not path:
            raise RouteMismatch()
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
                raise RouteMismatch()
        else:
            return int(arg)
    def render_argument(self, context, arg):
        return str(arg)