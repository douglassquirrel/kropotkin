from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from glob import glob
from httplib2 import Http
from json import dumps, load
from multiprocessing import Process
from os import access, path, walk, X_OK
from os.path import isdir, join
from subprocess import Popen
from tempfile import mkdtemp
from time import time
from urlparse import urlparse, parse_qsl

def store_fact(factspace_url, name, content):
    url = factspace_url + '/' + name
    headers = {'content-type':'application/x-www-form-urlencoded'}
    Http().request(url, "POST", content, headers)

def start_factspace(name, port, components_directory, kropotkin_url):
    print "Starting factspace %s on port %d using %s and %s"\
        % (name, port, components_directory, kropotkin_url)

    FACTS_LOCATION = mkdtemp()
    print "Storing facts in %s" % FACTS_LOCATION

    def save_fact(fact_type, content):
        tstamp = int(time())
        name = '.'.join([fact_type, str(tstamp), str(hash(content)), 'fact'])
        with open(path.join(FACTS_LOCATION, name), 'w') as fact_file:
            fact_file.write(content)

    def load_fact(fact_filename):
        with open(fact_filename, 'r') as fact_file:
            return load(fact_file)

    class handler(BaseHTTPRequestHandler):
        def do_GET(self):
            parsed_url = urlparse(self.path)
            url_path = parsed_url.path
            query_params = set(parse_qsl(parsed_url.query))

            if url_path == '/':
                self.give_response(200, '%s Factspace\n' % name)
            else:
                fact_type = url_path.split('/')[1]
                fact_files = glob(path.join(FACTS_LOCATION, fact_type + ".*"))
                facts = [load_fact(f) for f in fact_files]
                facts = [f for f in facts if query_params < set(f.viewitems())]
                self.give_response(200, dumps(facts), 'application/json')

        def do_POST(self):
            fact_type = (urlparse(self.path).path)[1:]
            length = int(self.headers.getheader('Content-Length'))
            content = self.rfile.read(length)

            if fact_type and content:
                save_fact(fact_type, content)
                self.give_response(200, '')
            else:
                self.give_response(400, '')

        def log_message(self, format, *args):
            return

        def give_response(self, resp_code, text, mime_type='text/plain'):
            self.send_response(resp_code)
            self.send_header('Access-Control-Allow-Origin', '*')
            self.send_header('Content-Length', len(text))
            self.send_header('Content-Type', mime_type + '; charset=utf-8')
            self.end_headers()
            if text:
                self.wfile.write(text)

    def run_server():
        server = HTTPServer(('', port), handler)
        server.serve_forever()

    process = Process(target=run_server)
    process.start()

    content = dumps({'name':name, 'port':port})
    if (name == 'Kropotkin'):
        save_fact('service-started', content)
    else:
        store_fact(kropotkin_url, 'service-started', content)

    def run_unique_executables_recursive(directory):
        nodes = set(glob(join(directory, "*")))
        dirs = set([f for f in nodes if isdir(f)])
        files = nodes - dirs

        for d in dirs:
            run_unique_executables_recursive(join(directory, d))

        executables = [f for f in files if access(join(directory, f), X_OK)]
        if (len(executables) != 1):
            print "%d executables in %s" % (len(executables), directory)
        else:
            print "Executing: %s" % executables[0]
            Popen(executables[0], cwd=directory)

    run_unique_executables_recursive(components_directory)

    process.join()
