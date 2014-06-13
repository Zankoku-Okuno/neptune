from chokma.http.pipeline import Endpoint, Route as URL, Resource as R, Renderer as View
from chokma.http.routing import Slug
from chokma import fs
from chokma.errors import HttpError
from chokma.http.context import EmptyResponse

class FileR(R):
    def GET(self, context, filename):
        try:
            filepath = fs.resolve('user', filename)
            return {'filepath': filepath}
        except ValueError:
            raise Http404(context)

    def POST(self, context, filename):
        filepath = fs.resolve('user', filename) + '.txt'
        with open(filepath, 'wb') as fp:
            fp.write(context.request.body)
        raise EmptyResponse(201)


class FileV(View):
    default_content_type = 'text/plain'
    def text(self, context, filepath):
        if context.response.code == 201:
            pass
        else:
            raise fs.Sendfile(filepath+'.txt')

endpoints = (
    Endpoint('file', FileR(), FileV(),
        URL(Slug('filename')) ),
)