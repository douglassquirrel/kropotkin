#!/usr/bin/python
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from itertools import count
from kropotkin import get_newest_fact
from SocketServer import ForkingMixIn
from urlparse import urlparse, parse_qsl

from component.get_component import get_component
from factspace.get_statements import get_statements
from factspace.store_statement import store_statement
from integration.get_computer_name import get_computer_name
from integration.perform_queue_command import perform_queue_command

def base(path, params, content, client_ip):
    test = get_newest_fact('kropotkin', 'constitution_element',
                           {'type': 'home_component'})
    if not test:
        return 200, 'Kropotkin HTTP\n', 'text/plain'

    home_fact = get_newest_fact('kropotkin', 'home_component', {})
    substitute_path = '/component/%s' % home_fact['name']
    return get_component(substitute_path, params, content, client_ip)

PORT=2001

class handler(BaseHTTPRequestHandler):
    routing = {('',               'GET'):  base,
               ('mycomputername', 'GET'):  get_computer_name,
               ('factspace',      'GET'):  get_statements,
               ('factspace',      'POST'): store_statement,
               ('component',      'GET'):  get_component,
               ('queue',          'POST'): perform_queue_command}

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
        client_ip = self.client_address[0]
        route = path.split('/')[1]

        try:
            responder = handler.routing[(route, verb)]
            code, content, mime_type = \
                responder(path, params, incoming_content, client_ip)
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
