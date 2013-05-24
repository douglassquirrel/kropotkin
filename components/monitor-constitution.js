function check_constitution() {
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    factspace = factspace_select.value;
    get_all_facts(factspace, 'constitution_element', {}, display_constitution);
}

var RADIO_HTML = '<input type="radio" name="const_inspect" ';
RADIO_HTML    +=        'value="VALUE" CHECKED/>';

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
        row = const_table.insertRow(-1);
        row.insertCell(0);
        row.insertCell(1);
        row.insertCell(2);
        row.insertCell(3);
    }
    facts.sort(function(f, g) {
        var a = f['type'];
        var b = g['type'];
        if (a<b) return -1;
        if (a>b) return 1;
        return 0;
    });
    for (var i=0; i < facts.length; i++) {
        row = const_table.rows[i];
        row.cells[0].innerHTML = facts[i]['type'];
        row.cells[1].innerHTML = facts[i]['keys'].join();
        row.cells[2].innerHTML = facts[i]['translation'];

        if (selected_element == facts[i]['type']) {
            checked = 'checked="checked"';
        } else {
            checked = '';
        }
        html = RADIO_HTML.replace('VALUE', facts[i]['type']);
        html = html.replace('CHECKED', checked);
        row.cells[3].innerHTML = html;
    }
}

setInterval(check_constitution, 1000);