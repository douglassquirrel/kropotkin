from httplib2 import Http

def store_fact(factspace_url, name, content):
    url = factspace_url + '/' + name
    headers = {'content-type':'application/x-www-form-urlencoded'}
    Http().request(url, "POST", content, headers)
