
# Some user agents don't use the '*/*' media range appropriately.
# Setting this True will ignore '*/*' in the HTTP Accept header.
ACCEPT_IGNORE_WILDCARD = True

CONTENT_TYPES = {
    'text/html': 'html',
    'application/json': 'json',
    'text/plain': 'text',
}