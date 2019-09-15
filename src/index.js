'use strict';

require("./styles.scss");
console.log(process.env.SECRET);

const {Elm} = require('./Main');
var app = Elm.Main.init({flags:  process.env.SECRET });
