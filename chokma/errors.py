_CODES = {
	200: "OK",

	404: "Not Found",
	406: "Not Acceptable",

	500: "Internal Server Error",
}

class HttpError(Exception):
	def __init__(self, request, msg=None):
		super().__init__(msg)
		self.status = "%d %s" % (self.code, _CODES[self.code])
		self.request = request


class Http404(HttpError): code = 404
class Http406(HttpError): code = 406
