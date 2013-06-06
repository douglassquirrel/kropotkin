function get_newest_n_statements(factspace, type, criteria, n, onresponse) {
    criteria['kropotkin_criteria'] = 'result-newest,number-' + n;
    var query_string = to_query_string(criteria);
    url = '/factspace/' + factspace + '/statement/' + type
        + '?' + to_query_string(criteria);
    http_request(url, 'GET', null, function(responseText) {
        onresponse(JSON.parse(responseText));
    });
}

function translate(str, dict) {
    r = /%\((.+?)\)s/;
    m = str.search(r);
    if (m == -1) return str;
    key = str.match(r)[1];
    value = dict[key];
    t = str.replace(r, value);
    start = t.slice(0, m + value.length);
    return start + translate(t.slice(m + value.length), dict);
}

function get_see_statements_element() {
    var radio_buttons = document.getElementsByName('const_inspect');
    for (var i=0; i<radio_buttons.length; i++) {
        if (radio_buttons[i].checked) return radio_buttons[i].value;
    }
    return null;
}

function check_statements() {
    var element = get_see_statements_element();
    if (element == null) return;
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    factspace = factspace_select.value;
    get_newest_n_statements(factspace, element, {}, 10,
                            display_statements);
}

function display_statements(statements) {
    var element = get_see_statements_element();

    var const_table = document.querySelectorAll('*[data-constitution]')[0];
    var translation = null;
    for (i=0; i<const_table.rows.length; i++) {
        if (const_table.rows[i].cells[0].innerHTML == element) {
            translation = const_table.rows[i].cells[2].innerHTML;
        }
    }
    if (null == translation) return;

    var statement_table = document.querySelectorAll('*[data-statements]')[0];
    for (i=0; i<10; i++) {
        var row_data = [];
        if (i < statements.length) {
            row_data.push('');
            row_data.push(statements[i]['kropotkin_confidence']);
            row_data.push('');
            row_data.push(translate(translation, statements[i]));
        } else {
            row_data = ['', '', '', '']
        }
        row = statement_table.rows[i];
        for (j=0; j<4; j++) {
            row.cells[j].innerHTML = row_data[j];
        }
    }
}

setInterval(check_statements, 1000);
report_deployment('monitor-statements.js');