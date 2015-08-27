import React, {Component, PropTypes} from 'react';
import moment from 'moment';

export const propTypes = {
  emit: PropTypes.any,
  blocks: PropTypes.any
};

export const defaultProps = {
  emit: () => console.log('emit is missing'),
  blocks: []
};

class Tableheader extends Component {
  handleDelete(block) {
    const event = new CustomEvent('deleteBlock', {
      'detail': block
    });
    this.props.emit(event);
  }
  render() {
    const ths = this.props.blocks.map(block => {
      const timeStart = moment(block.startHours + ':' + block.startMinutes,
        'HH:mm').format('LT');
      const timeEnd = moment(block.endHours + ':' + block.endMinutes,
        'HH:mm').format('LT');
      return (
        <th className="table-block" key={block.description}>
          <i className="close icon" onClick={this.handleDelete.bind(this, block)} />
          <div>
            <div>
              {block.description}
            </div>
            <div>
              {timeStart + ' - ' + timeEnd}
            </div>
          </div>
        </th>
      );
    });
    return (
      <thead>
        <tr>
          <th></th>
          {ths}
        </tr>
      </thead>
    );
  }
}

Tableheader.propTypes = propTypes;
Tableheader.defaultProps = defaultProps;

export default Tableheader;

