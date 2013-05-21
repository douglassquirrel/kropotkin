function check_constitution() {
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    factspace = factspace_select.value;
    get_all_facts(factspace, 'constitution_element', [], display_constitution);
}

function display_constitution(facts) {
    var const_table = document.querySelectorAll('*[data-constitution]')[0];
    var initial_table_rows = const_table.rows.length;
    for (i=0; i < initial_table_rows - facts.length; i++) {
        const_table.deleteRow(-1);
    }
    for (i=0; i < facts.length - initial_table_rows; i++) {
        row = const_table.insertRow(-1);
        row.insertCell(0);
        row.insertCell(1);
    }
    for (i=0; i < facts.length; i++) {
        row = const_table.rows[i];
        row.cells[0].innerHTML = facts[i]['type'];
        row.cells[1].innerHTML = facts[i]['keys'].join();
    }
}

setInterval(check_constitution, 1000);