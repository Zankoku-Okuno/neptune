class HttpError(Exception):
	def __init__(self, request, msg=None):
		super().__init__(msg)
		self.request = request


class Http404(HttpError): pass
class Http406(HttpError): pass
