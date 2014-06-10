class Resource:
    def go(self, context, **params):
        try:
            action = self.__getattribute__(context.request.method.upper())
        except AttributeError:
            raise Exception("TODO unsupported http method on this resource")
        else:
            for attr, value in action(context, **params).items():
                context.__setattr__(attr, value)
