import React from 'react';
import Tablecell from './Tablecell.js';

import _ from 'lodash';

var Tablerow = React.createClass({
    handleDelete() {
        this.props.emit(
            new CustomEvent('deleteRoom', {
                'detail': this.props.room
            })
        );
    },
    render() {
        var topics = _.zip(this.props.blocks, this.props.row)
                .map(([block, topic]) =>
                     <Tablecell block={block} emit={this.props.emit} key={block.start} room={this.props.room} topic={topic} />
                    );
        return (
                <tr>
                <td className="table-room">
                  <b>{this.props.room.name}</b>
                  <i className="close icon" onClick={this.handleDelete} />
                </td>
                {topics}
            </tr>
        );
    }
});

export default Tablerow;
