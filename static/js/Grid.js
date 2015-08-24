import React from 'react';
import Modal from 'react-modal';
import moment from 'moment';
import _ from 'lodash';

import Tableheader from 'babel!./components/Tableheader.js';
import Tablebody from 'babel!./components/Tablebody.js';

var Grid = React.createClass({
  emit(event) {
    this.getDOMNode().dispatchEvent(event);
  },
  render() {
    return (
      <table id='gridContainer' className="ui celled striped table">
        <Tableheader blocks={this.props.blocks} emit={this.emit}/>
        <Tablebody emit={this.emit} {...this.props}/>
      </table>
    );
  }
});

export default Grid;
