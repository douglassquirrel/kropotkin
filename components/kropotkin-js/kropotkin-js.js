if (!("console" in window) || !("log" in window.console)) {
    window.console = {"log": function(){}};
}

function store_fact(factspace, type, content) {
    var url = '/factspace/' + factspace + '/fact/' + type;
    http_request(url, 'POST', JSON.stringify(content), null);
}

function get_oldest_fact_and_stamp(factspace, type, criteria,
                                   stamp, onresponse) {
    var criteria = criteria.slice(0);
    criteria['kropotkin_criteria'] = 'stamp-' + stamp + ',result-oldest';
    get_first_fact(factspace, type, criteria, onresponse);
}

function get_newest_fact(factspace, type, criteria, onresponse) {
    var criteria = criteria.slice(0);
    criteria['kropotkin_criteria'] = 'result-newest';
    get_first_fact(factspace, type, criteria, onresponse);
}

function get_first_fact(factspace, type, criteria, onresponse) {
    get_all_facts(factspace, type, criteria, function(facts) {
         if (facts.length > 0) {
            onresponse(facts[0]);
        }
    });
}

function get_all_facts(factspace, type, criteria, onresponse) {
    var query_string = to_query_string(criteria);
    url = '/factspace/' + factspace + '/fact/' + type
        + '?' + to_query_string(criteria);
    http_request(url, 'GET', null, function(responseText) {
        onresponse(JSON.parse(responseText));
    });
}

function to_query_string(criteria) {
    criteria_strings = [];
    for (name in criteria) {
        criteria_strings.push(name + "=" + criteria[name]);
//        criteria_strings.push(encodeURIComponent(name) + "="
//                              + encodeURIComponent(criteria[name]));
    }
    return criteria_strings.join('&');
}

POST_MIME_TYPE = 'application/x-www-form-urlencoded'
function http_request(url, verb, content, onresponse) {
    var xmlhttp = new XMLHttpRequest();
    if (onresponse !== null) {
        xmlhttp.onreadystatechange = function() {
            if (xmlhttp.readyState != 4) { return; }

            var status       = xmlhttp.status;
            var responseText = xmlhttp.responseText;

            if (status != 200) {
                console.log("Request to " + url + " failed: "
                            + "status = " + status + ", "
                            + "response = '" + responseText + "'");
                return;
            }
            onresponse(xmlhttp.responseText);
        }
    }
    xmlhttp.open(verb, url, true);
    if (verb == 'POST') {
        xmlhttp.setRequestHeader("Content-type", POST_MIME_TYPE);
    }
    if (content == null) {
        xmlhttp.send();
    } else {
        xmlhttp.send(content);
    }
}