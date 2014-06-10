class Resource:
    def go(self, context, **params):
        try:
            action = self.__getattribute__(context.request.method.upper())
        except AttributeError:
            raise Exception("TODO unsupported http method on this resource")
        else:
            return action(context, **params)
