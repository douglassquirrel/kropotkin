# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, json, urlparse
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

class HTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        path = urlparse.urlparse(self.path).path.replace('/','.')
        outgoing_key = 'http_GET_request%s' % path
        incoming_key = 'http_GET_response%s' % path 
        mb = messageboard.MessageBoard()
        queue = mb.watch_for(keys=[incoming_key])
        mb.post(key=outgoing_key)
        message = mb.get_one_message(queue)
        if message:
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps(message.content['response']))
        else:
            self.send_response(501)
            self.send_header('Content-type', 'text/plain')
            self.end_headers()
            self.wfile.write('No timely response from server')
        return

try:
    mb = messageboard.MessageBoard()    
    server = HTTPServer(('', 8080), HTTPHandler)
    mb.post(key='process_ready.http_server')
    server.serve_forever()
except KeyboardInterrupt:
    server.socket.close()


