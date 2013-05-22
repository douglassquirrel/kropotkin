function check_components() {
    get_all_facts('kropotkin', 'component_deployed', [], display_components);
}

function display_components(facts) {
    var comp_table = document.querySelectorAll('*[data-components]')[0];
    var initial_table_rows = comp_table.rows.length;
    for (i=0; i < initial_table_rows - facts.length; i++) {
        comp_table.deleteRow(-1);
    }
    for (i=0; i < facts.length - initial_table_rows; i++) {
        row = comp_table.insertRow(-1);
        row.insertCell(0);
        row.insertCell(1);
        row.insertCell(2);
        row.insertCell(3);
    }
    facts.sort(function(f, g) {
        var a = f['name'];
        var b = g['name'];
        if (a<b) return -1;
        if (a>b) return 1;
        return 0;
    });
    for (i=0; i < facts.length; i++) {
        row = comp_table.rows[i];
        row.cells[0].innerHTML = facts[i]['name'];
        row.cells[1].innerHTML = facts[i]['location'];
        row.cells[2].innerHTML = facts[i]['identifier'];
    }
}

setInterval(check_components, 1000);