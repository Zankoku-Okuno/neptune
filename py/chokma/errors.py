from chokma import http

class BadConfiguration(Exception):
    pass

class RouteMismatch(Exception):
	pass


class HttpError(Exception):
	def __init__(self, context, msg=None, code=None):
		super().__init__(msg)
		self.context = context
		if code is not None:
			self.code = code


class Http404(HttpError): code = 404
class Http405(HttpError): code = 405
class Http406(HttpError): code = 406
