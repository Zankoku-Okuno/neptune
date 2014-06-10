from chokma.html.base import Element, NullaryElement, render

class Document:
    def __init__(self):
        self.title = None
        self._body = []

    def append(self, *elems):
        self._body += elems

    @property
    def html_gen(self):
        yield b'<!DOCTYPE html>\n<html><head>\n<meta charset="utf-8">\n'

        #TODO add'l meta

        if self.title is not None:
            yield b"<title>"
            yield from render(self.title)
            yield b"</title>\n"
        
        # TODO add'l js
        # TODO add'l css

        yield b"</head><body>\n"
        for elem in self._body:
            yield from render(elem)
        yield b"\n</body></html>"

class p(Element): tagname = b'p'

class div(Element): tagname = b'div'

class br(NullaryElement): tagname = b'br'