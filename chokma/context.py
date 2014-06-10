from chokma.path import sanitize


from chokma.config import config
_ignore_wildcard = config.ACCEPT_IGNORE_WILDCARD


class Context:
    """Aggregates request and response objects, as well as any other data the server needs to set.

    The lifetime of a context object is the entirety of a single HTTP request, and so is available throughout the framework.
    """
    def __init__(self, environ):
        self.request = Request(environ)
        self.response = Response()

    def set_header(self, key, value):
        """Add a header to the HTTP response."""
        self.response._headers.append((key, value))

class Request:
    def __init__(self, environ):
        self.scheme = environ['wsgi.url_scheme']
        self.path = sanitize(environ['PATH_INFO'], strict=False)
        self.method = environ['REQUEST_METHOD']
        self.accept = _parse_accept(environ['HTTP_ACCEPT'])

class Response:
    def __init__(self):
        self._headers = []
        self._body = None


def _parse_accept(input):
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
            if not _ignore_wildcard:
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

