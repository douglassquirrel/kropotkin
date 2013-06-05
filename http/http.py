#!/usr/bin/python
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from factspace.get_statements import get_statements
from factspace.store_statement import store_statement
from component.get_component import get_component
from itertools import count
from SocketServer import ForkingMixIn
from urlparse import urlparse, parse_qsl

def base(path, params, content):
    return 200, 'Kropotkin HTTP\n', 'text/plain'

PORT=2001

class handler(BaseHTTPRequestHandler):
    routing = {('', 'GET'):           base,
               ('factspace', 'GET'):  get_statements,
               ('factspace', 'POST'): store_statement,
               ('component', 'GET'):  get_component}

    def do_GET(self):
        self.route_request('GET')

    def do_POST(self):
        self.route_request('POST')

    def route_request(self, verb):
        parsed_url = urlparse(self.path)
        path = parsed_url.path
        params = dict(parse_qsl(parsed_url.query))
        length = int(self.headers.getheader('Content-Length') or 0)
        incoming_content = self.rfile.read(length)
        route = path.split('/')[1]

        try:
            responder = handler.routing[(route, verb)]
            code, content, mime_type = \
                responder(path, params, incoming_content)
        except KeyError:
            code = 404
            content = 'No route for %s with verb %s\n' % (route, verb)
            mime_type = 'text/plain'
        self.give_response(code, content, mime_type)

    def log_message(self, format, *args):
        return

    def give_response(self, resp_code, content, mime_type):
        self.send_response(resp_code)
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Content-Length', len(content))
        self.send_header('Content-Type', '%s; charset=utf-8' % mime_type)
        self.end_headers()
        if content:
            self.wfile.write(content)

class ForkingHTTPServer(ForkingMixIn, HTTPServer):
    pass

server = ForkingHTTPServer(('', PORT), handler)
server.serve_forever()
