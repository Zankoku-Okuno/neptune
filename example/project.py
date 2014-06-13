from os import path
from chokma.http.pipeline import Endpoint, Route as URL, Resource as R, Renderer as View
from chokma.http.routing import Slug
from chokma import fs, html
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
        raise fs.Sendfile(filepath+'.txt')

    def html(self, context, filepath):
        doc = html.Document()
        doc.title = path.basename(filepath)
        pars = []
        buffer = []
        with open(filepath+'.txt', 'rt') as fp:
            for line in fp.readlines():
                line = line.strip()
                if line:
                    buffer.append(line)
                else:
                    pars.append(' '.join(buffer))
                    buffer = []
        if buffer:
            pars.append(' '.join(buffer))
        del buffer
        doc.append(*map(html.p, pars))
        return html.render(doc)

endpoints = (
    Endpoint('file', FileR(), FileV(),
        URL(Slug('filename')) ),
)