from base64 import b64decode
from kropotkin import get_newest_fact

MIME_TYPES = {'component-tar':      'application/x-tar',
              'python-module':      'application/x-tar',
              'html':               'text/html',
              'javascript':         'application/javascript',
              'javascript-library': 'application/javascript',
              'ruby-gem':           'binary/octet-stream'}

def get_component(path, params, content):
    name = path.split('/')[2]
    component = get_newest_fact('kropotkin', 'component', {'name': name})
    if not component:
        return 404, 'No such component\n', 'text/plain'
    content = b64decode(component['bytes'])
    try:
        content_type = component['content_type']
        mime_type = MIME_TYPES[content_type]
    except KeyError:
        return 404, 'No mime type for %s\n' % content_type, 'text/plain'
    return 200, content, mime_type
