from html import escape
def render(x):
    if hasattr(x, 'html_gen'):
        yield from x.html_gen
    elif isinstance(x, str):
        yield escape(x).encode('utf-8')
    elif isinstance(x, bytes):
        yield x
    else:
        yield escape(str(x)).encode('utf-8')


class Element:
    def __init__(self, *elems):
        self._attrs = dict()
        self._inner = list(elems)

    def attr(self, key, value):
        if value is None:
            del self._attrs[key]
        self._attrs[key] = value
        return self
    def data(self, key, value):
        return self.attr('data-'+key, value)
    def id(self, value):
        return self.attr('id', value)
    def cls(self, *values):
        if 'class' in self._attrs:
            values = self._attrs['class'].split() + values
        return self.attr('class', ' '.join(values))

    def append(self, *elems):
        self._inner += elems
        return self

    @property
    def html_gen(self):
        yield b'<'
        yield self.tagname 
        for attr, value in self._attrs.items():
            yield b' '
            yield from render(attr)
            yield b'="'
            yield from render(value)
            yield b'"'
        yield b'>'
        for elem in self._inner:
            yield from render(elem)
        yield b'</' + self.tagname + b'>'

class NullaryElement(Element):
    def __init__(self):
        self._attrs = dict()

    def append(self, key):
        raise NotImplementedError()

    @property
    def html_gen(self):
        yield b'<'
        yield self.tagname
        if self._attrs:
            for attr, value in self._attrs.items():
                pass #STUB
            yield b' />'
        else:
            yield b'/>'

