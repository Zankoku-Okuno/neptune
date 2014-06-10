from os import path
from chokma.errors import BadConfiguration
from chokma.util.path import sanitize
from chokma.config import config

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
