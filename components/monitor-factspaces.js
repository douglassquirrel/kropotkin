function check_factspaces() {
    get_all_facts('kropotkin', 'factspace', {}, display_factspaces);
}

function display_factspaces(facts) {
    var factspace_select = document.querySelectorAll('*[data-factspace]')[0];
    var options = factspace_select.options;
    var option_values = []
    for (i=0; i<options.length; i++) {
        option_values.push(options[i].value)
    }
    for (i=0; i<facts.length; i++) {
        var name = facts[i]['name'];
        if (option_values.indexOf(name) < 0) {
            var option=document.createElement("option");
            option.text=name;
            options.add(option, null);
        }
    }
}

setInterval(check_factspaces, 1000);