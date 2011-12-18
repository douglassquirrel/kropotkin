# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, json, urlparse
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

class HTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        incoming_key = 'http%s' % urlparse.urlparse(self.path).path.replace('/','.')
        connection = messageboard.get_connection()
        messageboard.post(connection, key=incoming_key)

        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(self.path))
        return

try:
    connection = messageboard.get_connection()
    server = HTTPServer(('', 8080), HTTPHandler)
    messageboard.post(connection, key='process_ready.http')
    server.serve_forever()
except KeyboardInterrupt:
    server.socket.close()


