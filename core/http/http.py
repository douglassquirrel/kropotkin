#!/usr/bin/python
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from factspace.get_facts import get_facts
from factspace.store_fact import store_fact
from httplib2 import Http
from SocketServer import ThreadingMixIn
from urlparse import urlparse, parse_qsl

def base(path, params):
    return 'Kropotkin HTTP\n', 'text/plain'

PORT=2002

class handler(BaseHTTPRequestHandler):
    routing = {('', 'GET'):           base,
               ('factspace', 'GET'):  get_facts,
               ('factspace', 'POST'): store_fact}

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
            content, mime_type = responder(path, params, incoming_content)
            status = 200
        except KeyError:
            content = 'No route for %s with verb %s\n' % (route, verb)
            mime_type = 'text/plain'
            status = 404
        self.give_response(status, content, mime_type)

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

class ThreadedHTTPServer(ThreadingMixIn, HTTPServer):
    pass

server = ThreadedHTTPServer(('', PORT), handler)
server.serve_forever()
