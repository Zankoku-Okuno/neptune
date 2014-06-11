import os
from importlib import import_module
from chokma.errors import BadConfiguration

try:
    _module_name = os.environ['CHOKMA_CONFIG']
    config = import_module(_module_name)
except KeyError:
    raise BadConfiguration("Environment has no CHOKMA_CONFIG set.")
except ImportError:
    raise BadConfiguration("Can't import config module (%s)." % _module_name)