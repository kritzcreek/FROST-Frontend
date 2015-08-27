window.jQuery = window.$ = require('jquery');
require('rx');
require('rx-jquery');
require('moment')();

const Modal = require('react-modal');
const appElement = document.getElementById('menu');

Modal.setAppElement(appElement);
Modal.injectCSS();


window.Grid = require('./Grid.jsx').default;
window.Menu = require('./Menu.jsx').default;
window.Topics = require('./Topics.jsx').default;
window.React = require('react');

require('Openspace.Ui.Stream').main();
