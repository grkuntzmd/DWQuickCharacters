"use strict";

require("./index.html");

const Elm = require("./Main.elm");
const app = Elm.Main.fullscreen(Math.floor(Math.random() * 0xFFFFFFFF));

$(function() {
    $('[data-toggle="tooltip"]').tooltip()
})

function loadNames() {
    var values = [];
    for (var k in window.localStorage) {
        if (/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(k)) {
            const item = window.localStorage.getItem(k);
            values.push([k, JSON.parse(item).demographics.name]);
        }
    }
    return values;
}

app.ports.deleteItem.subscribe(function(id) {
    window.localStorage.removeItem(id);
    app.ports.getNames.send(loadNames());
});

app.ports.loadItem.subscribe(function(id) {
    app.ports.getItem.send(window.localStorage.getItem(id));
});

app.ports.loadNames.subscribe(function() {
    app.ports.getNames.send(loadNames());
});

app.ports.saveItem.subscribe(function(tuple) {
    window.localStorage.setItem(tuple[0], tuple[1]);
});

app.ports.showDialog.subscribe(function(id) {
    $(id).modal("show");
});