import React, {Component, PropTypes}from 'react';
import Tableheader from './components/Tableheader.jsx';
import Tablebody from './components/Tablebody.jsx';

export const propTypes = {
  blocks: PropTypes.any
};

class Grid extends Component {
  emit(event) {
    React.findDOMNode(this).dispatchEvent(event);
  }
  render() {
    return (
      <table id="gridContainer" className="ui celled striped table">
        <Tableheader blocks={this.props.blocks} emit={this.emit.bind(this)}/>
        <Tablebody emit={this.emit.bind(this)} {...this.props} />
      </table>
    );
  }
}

Grid.propTypes = propTypes;

export default Grid;

