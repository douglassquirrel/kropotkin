from kropotkin import _execute_queue_command
from urllib import unquote

def perform_queue_command(path, params, content, client_ip):
    path_elements = path.split('/')
    if len(path_elements) < 3:
        return 400, 'No queue command supplied', 'text/plain'
    elif len(path_elements) == 3:
        command, identifier = path_elements[2], None
    else:
        command, identifier = path_elements[2:4]
        identifier = unquote(identifier)
    input_ = content if content else None
    output = _execute_queue_command(command, input_, identifier)
    if output is not False:
        return 200, output, 'text/plain'
    else:
        return 400, 'Queue command %s failed' % command, 'text/plain'
