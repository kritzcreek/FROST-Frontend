import React, {Component, PropTypes} from 'react';
import Tablerow from './Tablerow.jsx';

import _ from 'lodash';

export const propTypes = {
  emit: PropTypes.any,
  blocks: PropTypes.any,
  rooms: PropTypes.any
};

const defaultProps = {
  emit: () => console.log('emit was missing'),
  blocks: [],
  rooms: []
};

class Tablebody extends Component {
  render() {
    const blocks = this.props.blocks;
    const rows = _.zip(this.props.rooms, this.props.grid)
      .map(row => {
        const room = _.head(row);
        return (
          <Tablerow blocks={blocks} emit={this.props.emit} key={room.name} room={room} row={_.tail(row)[0]} />
        );
      });
    return (
      <tbody>
        {rows}
      </tbody>
    );
  }
}

Tablebody.propTypes = propTypes;
Tablebody.defaultProps = defaultProps;

export default Tablebody;

