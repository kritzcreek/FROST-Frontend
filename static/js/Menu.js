import React from 'react';
import _ from 'lodash';
import AddTopicModal from 'babel!./components/AddTopicModal.js';
import RemoveButton from 'babel!./components/RemoveButton.js';
import AddBlockModal from 'babel!./components/AddBlockModal.js';
import AddRoomModal from 'babel!./components/AddRoomModal.js';


var Menu = React.createClass({
    emit(event) {
        this.getDOMNode().dispatchEvent(event);
    },
    render() {
        let classes = "ui segment";
        let el = (<AddTopicModal emit={this.emit} topicTypes={this.props.topicTypes} />);
        if(window.switch === true){
            classes = "ui segment menu-delete";
            el = (<RemoveButton emit={this.emit} />);
        }
    return (
      <div id="menuContainer" className={classes}>
            <AddRoomModal emit={this.emit}/>
            {el}
            <AddBlockModal emit={this.emit}/>
      </div>
    );
  }
});

export default Menu;
