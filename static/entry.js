window.jQuery = window.$ = require("jquery");
require('rx');
require('rx-jquery');
require('moment')();

window.Grid = require('babel!./grid.js');
window.Menu = require('babel!./menu.js');
window.Topics = require('babel!./topics.js');
window.React = require('react');

require('Openspace.Ui.Stream').main();
