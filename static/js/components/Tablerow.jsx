import React, {Component, PropTypes} from 'react';
import Tablecell from './Tablecell.jsx';

import _ from 'lodash';

export const propTypes = {
  emit: PropTypes.any,
  room: PropTypes.any,
  row: PropTypes.any,
  blocks: PropTypes.any
};

export const defaultProps = {
  emit: () => console.log('expected emit'),
  room: null,
  row: null,
  blocks: []
};

class Tablerow extends Component {
  handleDelete() {
    this.props.emit(
      new CustomEvent('deleteRoom', {
        'detail': this.props.room
      })
    );
  }
  render() {
    const topics = _.zip(this.props.blocks, this.props.row)
      .map(([block, topic]) =>
        <Tablecell block={block} emit={this.props.emit} key={block.start} room={this.props.room} topic={topic} />
      );
    return (
      <tr>
        <td className="table-room">
          <strong>{this.props.room.name}</strong>
          <i className="close icon" onClick={this.handleDelete.bind(this)} />
        </td>
        {topics}
      </tr>
    );
  }
}

Tablerow.propTypes = propTypes;
Tablerow.defaultProps = defaultProps;

export default Tablerow;
