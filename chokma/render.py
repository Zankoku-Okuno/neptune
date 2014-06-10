#FIXME allow configuration
CONTENT_TYPES = (
    ('text/html', 'html'),
    ('application/json', 'json'),
    ('text/plain', 'text'),
)
CONTENT_TYPES = dict(CONTENT_TYPES)


from chokma.errors import Http406


class Renderer:
    def go(self, context):
        ct = self._negotiate(context)
        context.set_header('Content-Type', ct)
        context.response._body = self.__getattribute__(CONTENT_TYPES[ct])(context)
        return context

    #FIXME probably not RFC compilant
    def _negotiate(self, context):
        for client in context.request.accept:
            for server in self.targets:
                if client == ('*', '*'):
                    try:
                        return self.default_content_type
                    except AttributeError:
                        pass # continue searching
                elif client[1] == '*' and client[0] == server[0]:
                    return '/'.join(server)
                elif client == server:
                    return '/'.join(server)
        else:
            raise Http406(context.request)

    @property
    def targets(self):
        if hasattr(self, '_targets') and self._targets is not None:
            return self._targets
        acc = []
        for ct, method in CONTENT_TYPES.items():
            if hasattr(self, method):
                acc.append(tuple(ct.split('/')))
        acc = tuple(acc)
        self._targets = acc
        return acc

