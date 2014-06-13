from chokma.util.path import sanitize


class Context:
    """Aggregates request and response objects, as well as any other data the server needs to set.

    The lifetime of a context object is the entirety of a single HTTP request, and so is available throughout the framework.
    """
    def __init__(self, environ):
        self.request = Request(environ)
        self.response = Response()

    def has_header(self, key):
        return self.response.has_header(key)
    def set_header(self, key, value):
        """Add a header to the HTTP response."""
        self.response.set_header(key, value)

class Request:
    def __init__(self, environ):
        self.scheme = environ['wsgi.url_scheme']
        self.path = environ['PATH_INFO'].strip('/').split('/')
        self.method = environ['REQUEST_METHOD']
        self.accept = _parse_accept(environ['HTTP_ACCEPT'])
        if environ['CONTENT_LENGTH']:
            self.body = environ['wsgi.input'].read(int(environ['CONTENT_LENGTH'])) #FIXME make it lazy 

class Response:
    def __init__(self, code=200):
        self._headers = []
        self.body = None
        self.code = code

    @property
    def headers(self):
        return self._headers

    def has_header(self, key):
        for k, _ in self._headers:
            if key == k:
                return True
        else:
            return False
    def set_header(self, key, value):
        self._headers.append((key, value))

class EmptyResponse(Exception):
    def __init__(self, code=200):
        self.code = code


def _parse_accept(input):
    from chokma.config import config
    output = dict()
    for media_range in input.split(','):
        # parse out media type and quality value
        if ';' in media_range:
            media_type, qval = media_range.split(';')
            qval = float(qval[2:])
        else:
            media_type, qval = media_range, 1.0
        # normalize media_type
        media_type = tuple(media_type.strip().split('/'))
        # add to accumulator
        if qval not in output:
            output[qval] = ([], [], [])
        if media_type == ('*', '*'):
            if not config.ACCEPT_IGNORE_WILDCARD:
                output[qval][2].append(media_type)
        elif media_type[1] == '*':
            output[qval][1].append(media_type)
        else:
            output[qval][0].append(media_type)
    # merge down and sort accumulators
    acc = []
    for _, cts in sorted(output.items(), reverse=True):
        acc += cts[0]
        acc += cts[1]
        acc += cts[2]
    return acc

