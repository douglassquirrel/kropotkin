def get_computer_name(path, params, content, client_ip):
    if client_ip == '127.0.0.1':
        name = 'localhost'
    else:
        name = client_ip
    return 200, name, 'text/plain'
