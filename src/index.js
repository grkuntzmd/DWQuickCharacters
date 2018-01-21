"use strict";

require("./index.html");

const Elm = require("./Main.elm");
const app = Elm.Main.fullscreen(Math.floor(Math.random() * 0xFFFFFFFF));

$(function() {
    $('[data-toggle="tooltip"]').tooltip()
})

app.ports.showDialog.subscribe(function(id) {
    $(id).modal("show");
});