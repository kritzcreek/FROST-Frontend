import React from 'react';
import _ from 'lodash';
import AddTopicModal from 'babel!./components/AddTopicModal.js';
import RemoveButton from 'babel!./components/RemoveButton.js';


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
            <button className="ui button"> Add Room</button>
            {el}
            <button className="ui button"> Add Block</button>
      </div>
    );
  }
});

export default Menu;
