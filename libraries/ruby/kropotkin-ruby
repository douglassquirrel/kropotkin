#!/usr/bin/ruby
require 'net/http'
require 'json'

def make_query_function(confidence, stamped, which)
  if stamped
    return lambda {|factspace, type, criteria, stamp| \
      get_statements(confidence, which, stamp, factspace, type, criteria)}
  else
    return lambda {|factspace, type, criteria| \
      get_statements(confidence, which, false, factspace, type, criteria)}
  end
end

def get_oldest_fact_and_stamp(factspace, type, criteria, stamp)
  f = make_query_function('fact', true, 'oldest')
  f.call(factspace, type, criteria, stamp)
end

def get_newest_fact(factspace, type, criteria)
  f = make_query_function('fact', false, 'newest')
  f.call(factspace, type, criteria)
end

def get_all_facts(factspace, type, criteria)
  f = make_query_function('fact', false, 'all')
  f.call(factspace, type, criteria)
end

def store_fact(factspace, type, content)
  store_statement('fact', factspace, type, content)
end

def store_opinion(factspace, type, content)
  store_statement('opinion', factspace, type, content)
end

def create_factspace(name, timeout=5)
  if !store_fact('kropotkin', 'factspace_wanted', {'name' => name})
    return false
  end
  finish = Time.now.to_i + timeout
  while Time.now.to_i < finish
    if get_newest_fact('kropotkin', 'factspace', {'name' => name})
      return true
    end
  end
  return false
end

def store_statement(confidence, factspace, type, content)
  kropotkin_url = ENV['KROPOTKIN_URL']
  url = '%s/factspace/%s/%s/%s' % [kropotkin_url, factspace, confidence, type]
  uri = URI.parse(url)
  http = Net::HTTP.new(uri.host, uri.port)
  resp = http.post(url, JSON.dump(content))
  return (resp.code == '200')
end

def get_statements(confidence, which, stamp, factspace, type, criteria)
  kropotkin_criteria_list = []
  if stamp
    kropotkin_criteria_list.push('stamp-' + stamp)
  end
  if which != 'all'
    kropotkin_criteria_list.push('result-' + which)
  end

  criteria = Hash[criteria]
  if kropotkin_criteria_list.length > 0
    criteria['kropotkin_criteria'] = kropotkin_criteria_list.join(',')
  end
  statements = get_all_statements(confidence, factspace, type, criteria)
  if which == 'all'
    return statements
  else
    return statements[0]
  end
end

def get_all_statements(confidence, factspace, type, criteria)
  kropotkin_url = ENV['KROPOTKIN_URL']
  params = URI.encode_www_form(criteria)
  url = '%s/factspace/%s/%s/%s?%s' %
    [kropotkin_url, factspace, confidence, type, params]
  uri = URI.parse(url)
  http = Net::HTTP.new(uri.host, uri.port)
  resp = http.get(url)
  if resp.code == '200'
    return JSON.parse(resp.body)
  else
    raise 'Unexpected response from server: %s' % resp.code
  end
end
