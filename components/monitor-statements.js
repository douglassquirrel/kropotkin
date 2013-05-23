function get_newest_n_statements(factspace, type, criteria, n, onresponse) {
    criteria['kropotkin_criteria'] = 'result-newest,number-' + n;
    var query_string = to_query_string(criteria);
    url = '/factspace/' + factspace + '/statement/' + type
        + '?' + to_query_string(criteria);
    http_request(url, 'GET', null, function(responseText) {
        onresponse(JSON.parse(responseText));
    });
}

function check_statements() {
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    factspace = factspace_select.value;
    get_newest_n_statements(factspace, 'constitution_element', [], 10,
                            display_statements);
}

function display_statements(statements) {
    console.log(statements);
    var statement_table = document.querySelectorAll('*[data-statements]')[0];
    for (i=0; i < 10; i++) {
        var row_data = [];
        if (i < statements.length) {
            row_data.push('');
            row_data.push(statements[i]['kropotkin_confidence']);
            row_data.push('');
            row_data.push('translation goes here');
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