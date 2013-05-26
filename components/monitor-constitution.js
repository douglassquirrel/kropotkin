var CONST_RADIO_HTML = '<input type="radio" name="const_inspect" ';
CONST_RADIO_HTML    +=        'value="VALUE" CHECKED/>\n';

var CONST_BUTTON_HTML = '<button onclick="setup_fact_insertion(INDEX)">';
CONST_BUTTON_HTML    += 'Insert Fact</button>\n';

var FACT_TITLE_HTML = '<h3>Fact of type TYPE</h3>\n';
var FACT_FIELD_HTML = '<label for="fact-ID">NAME</label>';
FACT_FIELD_HTML    += '<input type="text" name="NAME" id="fact-ID"/><br>\n'
var FACT_BUTTON_HTML = '<button onclick="submit_fact(\'TYPE\')">';
FACT_BUTTON_HTML    += 'Submit Fact</button>\n';

function check_constitution() {
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    factspace = factspace_select.value;
    get_all_facts(factspace, 'constitution_element', {}, display_constitution);
}

function get_see_statements_element() {
    var radio_buttons = document.getElementsByName('const_inspect');
    for (var i=0; i<radio_buttons.length; i++) {
        if (radio_buttons[i].checked) return radio_buttons[i].value;
    }
    return null;
}

function display_constitution(facts) {
    var selected_element = get_see_statements_element();
    var const_table = document.querySelectorAll('*[data-constitution]')[0];
    var initial_table_rows = const_table.rows.length;
    for (var i=0; i < initial_table_rows - facts.length; i++) {
        const_table.deleteRow(-1);
    }
    for (var i=0; i < facts.length - initial_table_rows; i++) {
        var row = const_table.insertRow(-1);
        for (var j=0; j<5; j++) { row.insertCell(j); }
    }
    facts.sort(function(f, g) {
        var a = f['type'];
        var b = g['type'];
        if (a<b) return -1;
        if (a>b) return 1;
        return 0;
    });
    for (var i=0; i < facts.length; i++) {
        var row = const_table.rows[i];
        row.cells[0].innerHTML = facts[i]['type'];
        row.cells[1].innerHTML = JSON.stringify(facts[i]['keys']);
        row.cells[2].innerHTML = facts[i]['translation'];

        if (selected_element == facts[i]['type']) {
            checked = 'checked="checked"';
        } else {
            checked = '';
        }
        var radio_html = CONST_RADIO_HTML.replace('VALUE', facts[i]['type']);
        radio_html = radio_html.replace('CHECKED', checked);
        row.cells[3].innerHTML = radio_html;

        var button_html = CONST_BUTTON_HTML.replace('INDEX', i);
        row.cells[4].innerHTML = button_html;
    }
}

function setup_fact_insertion(index) {
    var const_table = document.querySelectorAll('*[data-constitution]')[0];
    var rows = const_table.rows[index];
    var type = rows.cells[0].innerHTML;
    var keys = JSON.parse(rows.cells[1].innerHTML);

    var fact_div = document.querySelectorAll('*[data-insert-fact]')[0];
    var html = FACT_TITLE_HTML.replace("TYPE", type);
    for (var i=0; i<keys.length; i++) {
        html += FACT_FIELD_HTML.replace(/ID/g, i).replace(/NAME/g, keys[i]);
    }
    html += FACT_BUTTON_HTML.replace('TYPE', type);
    fact_div.innerHTML = html;
}

function submit_fact(type) {
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    factspace = factspace_select.value;

    var labels = document.querySelectorAll('*[data-insert-fact]>label');
    var content = '{';
    for (var i=0; i<labels.length; i++) {
        var name = labels[i].innerHTML;
        var id = labels[i].htmlFor;
        var input = document.getElementById(id);
        var value = input.value;
        content += '"NAME":VAL,'.replace('NAME', name).replace('VAL', value);
    }
    content = content.replace(/,$/, '}');

    store_fact(factspace, type, JSON.parse(content));
}

setInterval(check_constitution, 1000);