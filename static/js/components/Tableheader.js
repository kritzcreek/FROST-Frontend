import React from 'react';
import moment from 'moment';

import AddBlockModal from 'babel!./AddBlockModal.js';

var Tableheader = React.createClass({
    handleDelete(block) {
        var event = new CustomEvent('deleteBlock', {
            'detail': block
        });
        this.props.emit(event);
    },
    render() {
        var ths = this.props.blocks.map(block => {
            var timeStart = moment(block.startHours + ':' + block.startMinutes, 'HH:mm').format('LT');
            var timeEnd = moment(block.endHours + ':' + block.endMinutes, 'HH:mm').format('LT');
            return (
                    <th key={block.description}>
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
                <th><AddBlockModal emit={this.props.emit}/></th>
                </tr>
                </thead>
        );
    }
});

export default Tableheader;
