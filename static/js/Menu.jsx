import React, {Component, PropTypes} from 'react';
import NewTopicModal from './components/NewTopicModal.jsx';
import RemoveButton from './components/RemoveButton.jsx';
import NewBlockModal from './components/NewBlockModal.jsx';
import NewRoomModal from './components/NewRoomModal.jsx';

import Rx from 'rx';

export const propTypes = {
  topicTypes: PropTypes.any
};

class Menu extends Component {
  constructor() {
    super();
    this.state = {delete: false};
    this.emit = this.emit.bind(this);
  }
  componentDidMount() {
    Rx.Observable.fromEvent(document, 'setDrag')
      .forEach(() => {
        this.setState({delete: true});
      });
    Rx.Observable.fromEvent(document, 'unsetDrag')
      .forEach(() => {
        this.setState({delete: false});
      });
  }
  emit(event) {
    React.findDOMNode(this).dispatchEvent(event);
  }
  render() {
    let el;
    if (this.state.delete) {
      el = (
        <div id="menuContainer" className="ui segment menu-delete">
          <RemoveButton emit={this.emit} />
        </div>
      );
    } else {
      el = (
        <div id="menuContainer" className="ui segment">
          <NewRoomModal emit={this.emit}/>
          <NewTopicModal emit={this.emit} topicTypes={this.props.topicTypes} />
          <NewBlockModal emit={this.emit}/>
        </div>
      );
    }

    return el;
  }
}

Menu.propTypes = propTypes;

export default Menu;
