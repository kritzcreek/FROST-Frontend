import React from 'react';
import Tablerow from 'babel!./Tablerow.js';

import _ from 'lodash';

var Tablebody = React.createClass({
    render() {
        var blocks = this.props.blocks;
        var rows = _.zip(this.props.rooms, this.props.grid).map(row => {
            var room = _.head(row);
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
});

export default Tablebody;
