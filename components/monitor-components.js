var COMP_BUTTON_HTML = '<button onclick="get_code(\'NAME\')">Code</button>\n';
var CODE_HEADER_HTML = '<h2>NAME</h2>\n';
var CODE_TEXT_HTML = '<p><pre><code>TEXT</code></pre></p>\n';

function check_components() {
    get_all_facts('kropotkin', 'component_deployed', {}, display_components);
}

function display_components(facts) {
    var comp_table = document.querySelectorAll('*[data-components]')[0];
    var initial_table_rows = comp_table.rows.length;
    for (var i=0; i<initial_table_rows-facts.length; i++) {
        comp_table.deleteRow(-1);
    }
    for (i=0; i<facts.length-initial_table_rows; i++) {
        row = comp_table.insertRow(-1);
        for (var j=0; j<4; j++) { row.insertCell(j); }
    }
    facts.sort(function(f, g) {
        var a = f['name'];
        var b = g['name'];
        if (a<b) return -1;
        if (a>b) return 1;
        return 0;
    });
    for (var i=0; i<facts.length; i++) {
        var row = comp_table.rows[i];
        row.cells[0].innerHTML = facts[i]['name'];
        row.cells[1].innerHTML = facts[i]['location'];
        row.cells[2].innerHTML = facts[i]['identifier'];
        button_html = COMP_BUTTON_HTML.replace('NAME', facts[i]['name']);
        row.cells[3].innerHTML = button_html;
    }
}

function get_code(name) {
    get_newest_fact('kropotkin', 'component', {'name': name}, show_code);
}

function show_code(fact) {
    if (fact == null) { return; }
    tar = atob(fact['bytes']);
    files = parseTar(tar);
    html = '';
    for (var i=0; i<files.length; i++) {
        html += CODE_HEADER_HTML.replace('NAME', files[i].filename);
        html += CODE_TEXT_HTML.replace('TEXT', files[i].data);
    }

    var code_div = document.querySelectorAll('*[data-code]')[0];
    code_div.innerHTML = html;
}

subscribe('kropotkin', 'component_deployed');
setInterval(check_components, 1000);
report_deployment('monitor-components.js');