from base64 import b64decode
from kropotkin import get_newest_fact

MIME_TYPES = {'html': 'text/html',
              'js':   'application/javascript'}

def get_component(path, params, content):
    name = path.split('/')[2]
    component = get_newest_fact('kropotkin', 'component', {'name': name})
    if not component:
        return 404, 'No such component\n', 'text/plain'
    content = b64decode(component['bytes'])
    mime_type = 'application/octet-stream' # improve
    return 200, content, mime_type
