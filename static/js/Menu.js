import React from 'react';
import _ from 'lodash';
import NewTopicModal from 'babel!./components/NewTopicModal.js';
import RemoveButton from 'babel!./components/RemoveButton.js';
import NewBlockModal from 'babel!./components/NewBlockModal.js';
import NewRoomModal from 'babel!./components/NewRoomModal.js';

var Menu = React.createClass({
    emit(event) {
        this.getDOMNode().dispatchEvent(event);
    },
    render() {
        let classes = "ui segment";
        let el = (<NewTopicModal emit={this.emit} topicTypes={this.props.topicTypes} />);
        if(window.switch === true){
            classes = "ui segment menu-delete";
            el = (<RemoveButton emit={this.emit} />);
        }
    return (
      <div id="menuContainer" className={classes}>
            <NewRoomModal emit={this.emit}/>
            {el}
            <NewBlockModal emit={this.emit}/>
      </div>
    );
  }
});

export default Menu;
