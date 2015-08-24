window.jQuery = window.$ = require("jquery");
require('rx');
require('rx-jquery');
require('moment')();

var Modal = require('react-modal');

var appElement = document.getElementById("menu");
Modal.setAppElement(appElement);
Modal.injectCSS();


window.Grid = require('babel!./Grid.js');
window.Menu = require('babel!./Menu.js');
window.Topics = require('babel!./Topics.js');
window.React = require('react');

require('Openspace.Ui.Stream').main();
