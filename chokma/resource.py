class Resource:
    def __init__(self, name):
        self.name = name

    def go(self, context, **params):
        try:
            action = self.__getattribute__(context.request.method.upper())
        except AttributeError:
            raise Exception("TODO unsupported http method on this resource")
        else:
            for attr, value in action(context, **params).items():
                context.__setattr__(attr, value)
            return self._renderer.go(context)

class AnonymousResource(Resource):
    def __init__(self):
        super().__init_(None)

