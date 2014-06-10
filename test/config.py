
# Some user agents don't use the '*/*' media range appropriately.
# Setting this True will ignore '*/*' in the HTTP Accept header.
ACCEPT_IGNORE_WILDCARD = False

CONTENT_TYPES = {
    'text/html': 'html',
    'application/json': 'json',
    'text/plain': 'text',
    'application/octet-stream': 'bytes',
}

from os import path
LOCATIONS = {
	'test': path.dirname(path.abspath(__file__))
}