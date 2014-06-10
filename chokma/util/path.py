from os import path

_illegal_parts = {'', '.', '..'}
def sanitize(strpath, strict=True, split=False):
    """Return a normalized, relative path if possible."""
    parts = []
    for part in strpath.strip('/').split('/'):
        if part in _illegal_parts:
            if strict:
                raise ValueError(strpath)
        else:
            parts.append(part)
    if not parts:
        raise ValueError(strpath)
    return parts if split else path.join(*parts) 
