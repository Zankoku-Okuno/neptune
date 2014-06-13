from os import path
from chokma.util.path import sanitize
from chokma.config import config
from chokma.errors import BadConfiguration

if not hasattr(config, 'LOCATIONS'):
    raise BadConfiguration("Module chokma.fs requires LOCATIONS setting.")

def resolve(location_name, strpath):
    return path.join(config.LOCATIONS[location_name], sanitize(strpath))

def desolve(location_name, strpath):
    leading_path = config.LOCATIONS[location_name]
    if not leading_path.endswith('/'):
        leading_path += '/'
    if strpath.startswith(leading_path):
        return strpath[len(leading_path):]
    else:
        raise ValueError("Can't remove location from path.") #TODO better msg


class Sendfile(Exception):
    def __init__(self, filepath):
        self.filepath = filepath

def file_chunks(filepath, mode='rb', chunk_size=1024, xform=None):
    with open(filepath, mode) as fp:
        if xform is None:
            while True:
                data = fp.read(chunk_size)
                if not data:
                    break
                yield data
        else:
            while True:
                data = fp.read(chunk_size)
                if not data:
                    break
                yield xform(data)
