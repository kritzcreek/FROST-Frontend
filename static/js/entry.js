window.jQuery = window.$ = require("jquery");
require('rx');
require('rx-jquery');
require('moment')();

var Modal = require('react-modal');

var appElement = document.getElementById("menu");
Modal.setAppElement(appElement);
Modal.injectCSS();


window.Grid = require('./Grid.js');
window.Menu = require('./Menu.js');
window.Topics = require('./Topics.js');
window.React = require('react');

require('Openspace.Ui.Stream').main();
