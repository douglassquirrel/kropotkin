POST_MIME_TYPE = "application/x-www-form-urlencoded"
function store_fact(factspace, type, content) {
    var xmlhttp = new XMLHttpRequest();
    var url = '/factspace/' + factspace + '/' + type;
    xmlhttp.open("POST", url, true);
    xmlhttp.setRequestHeader("Content-type", POST_MIME_TYPE);
    xmlhttp.send(JSON.stringify(content));
}