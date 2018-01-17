"use strict";

require("./index.html");

const Elm = require("./Main.elm");
const app = Elm.Main.fullscreen(Math.floor(Math.random() * 0xFFFFFFFF));