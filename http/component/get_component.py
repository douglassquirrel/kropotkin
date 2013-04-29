from base64 import b64decode
from contextlib import closing
from kropotkin import get_newest_fact
from StringIO import StringIO
from tarfile import open as taropen

MIME_TYPES = {'html': 'text/html',
              'js':   'application/javascript'}

def get_component(path, params, content):
    name = path.split('/')[2]
    component = get_newest_fact('kropotkin', 'component', {'name': name})
    if not component:
        return 404, 'No such component\n', 'text/plain'
    tar_data = b64decode(component['tar'])
    with closing(StringIO(tar_data)) as buffer:
        with taropen(mode='r', fileobj=buffer) as tar:
            if len(tar.getnames()) == 1:
                only_file = tar.getnames()[0]
                content = tar.extractfile(only_file).read()
                file_type = only_file.split('.')[1]
                mime_type = MIME_TYPES[file_type]
            else:
                content = tar_data
                mime_type = 'application/x-tar'
    return 200, content, mime_type
