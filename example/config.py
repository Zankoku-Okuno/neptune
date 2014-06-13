
# Some user agents don't use the '*/*' media range appropriately.
# Setting this True will ignore '*/*' in the HTTP Accept header.
ACCEPT_IGNORE_WILDCARD = False

CONTENT_TYPES = {
    'text/html': 'html',
    'application/json': 'json',
    'text/plain': 'text',
    'application/octet-stream': 'binary',
}

from os import path
HERE = path.dirname(path.abspath(__file__))
LOCATIONS = {
	'user': path.join(HERE, 'upload'),
}

ENDPOINTS = 'example.project'
