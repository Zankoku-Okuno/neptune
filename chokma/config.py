import os
from os import path
import imp
from chokma.errors import BadConfiguration

config = None

try:
    config_path = os.environ['CHOKMA_CONFIG']
except KeyError:
    raise BadConfiguration("Environment has no CHOKMA_CONFIG set.")
else:
    config_dir, config_file = path.split(os.environ['CHOKMA_CONFIG'])
    if config_file.endswith(".py"):
        config_file = config_file[:-3]
    else:
        raise BadConfiguration("CHOKMA_CONFIG (%s) is not a python file." % config_path)
    config_fp, config_pathname, description = imp.find_module(config_file, [config_dir])
    if config_fp is None:
        raise BadConfiguration("Could not find CHOKMA_CONFIG (%s)." % config_path)
    try:
        config = imp.load_module(config_file, config_fp, config_pathname, description)
    finally:
        config_fp.close()
