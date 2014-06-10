from os import path
from chokma.config import config

def resolve(location_name, strpath):
    return path.join(config.LOCATIONS[location_name], strpath)
def desolve(location_name, strpath):
    leading_path = config.LOCATIONS[location_name]
    if not leading_path.endswith('/'):
        leading_path += '/'
    if strpath.startswith(leading_path):
        return strpath[len(leading_path):]
    else:
        raise ValueError("Can't remove location from path.")

_illegal_parts = {'', '.', '..'}
def sanitize(strpath, strict=True):
    """Return a normalized, relative path if possible."""
    parts = []
    for part in strpath.split('/'):
        if part in _illegal_parts:
            if strict:
                raise Exception("Illegal path part.")
        else:
            parts.append(part)
    if not parts:
        raise ValueError(strparts)
    return path.join(parts)
