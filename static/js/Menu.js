import React from 'react';
import NewTopicModal from 'babel!./components/NewTopicModal.js';
import RemoveButton from 'babel!./components/RemoveButton.js';
import NewBlockModal from 'babel!./components/NewBlockModal.js';
import NewRoomModal from 'babel!./components/NewRoomModal.js';

import Rx from 'rx';

var Menu = React.createClass({
    getInitialState() {
        return {delete: false};
    },
    componentDidMount() {
        Rx.Observable.fromEvent(document, 'setDrag')
        .forEach((e) => {
            this.setState({delete: true});
        });


        Rx.Observable.fromEvent(document, 'unsetDrag')
            .forEach((e) => {
                this.setState({delete: false});
            });
    },
    emit(event) {
        this.getDOMNode().dispatchEvent(event);
    },
    render() {
        if(this.state.delete){
          return (
           <div id="menuContainer" className="ui segment menu-delete">
             <RemoveButton emit={this.emit} />
           </div>

          );
        } else {
          return (
            <div id="menuContainer" className="ui segment">
              <NewRoomModal emit={this.emit}/>
              <NewTopicModal emit={this.emit} topicTypes={this.props.topicTypes} />
              <NewBlockModal emit={this.emit}/>
            </div>
          );
        }
  }
});

export default Menu;
