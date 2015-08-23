import React from 'react';
import _ from 'lodash';
import Modal from 'react-modal'

var appElement = document.getElementById("menu");
Modal.setAppElement(appElement);
Modal.injectCSS();

var AddModal = React.createClass({
  handleClick() {
    var event = new CustomEvent('addTopic', {
      'detail': {
          description: $('#descriptionInput').val(),
          typ: $('#topicTypeInput').val(),
          host: $('#hostInput').val()
      }
    });
    this.props.emit(event);
    this.props.close();
  },
  render() {
    var options = this.props.topicTypes.map(topicType =>
        <option key={topicType} value={topicType}>{topicType}</option>
    );
    return (
      <Modal isOpen={this.props.show} onRequestClose={this.props.close}>
        <h3 className="ui header">Add Topic</h3>
        <form className="ui form">
          <div className="field">
            <label>Thema</label>
            <input id="descriptionInput" type="text"/>
          </div>
          <div className="field">
            <label>Host</label>
            <input id="hostInput" type="text"/>
          </div>
          <div className="field">
            <label>Typ</label>
            <select id="topicTypeInput" type="select" className="ui fluid dropdown">
              {options}
            </select>
          </div>
          <button type="button" onClick={this.props.close} className="ui black deny button"> Close </button>
          <button type="button" onClick={this.handleClick} className="ui green button"> Hinzufügen </button>
        </form>
      </Modal>
    );
  }
});

var OpenAddModal = React.createClass({
    getInitialState(){
        return { show: false };
    },
    close(){
        this.setState({show: false});
    },
    open(){
        this.setState({show: true});
    },
    render() {
        return (
          <div>
            <button onClick={this.open} className="ui green button">
              Neues Thema
            </button>
            <AddModal close={this.close} show={this.state.show} onHide={this.close} {...this.props}/>
          </div>
        );
    }
});

var RemoveButton = React.createClass({
  getInitialState() {
    return {
      dragOver: false
    };
  },
  handleDragEnter() {
    this.setState({
      dragOver: true
    });
  },
  handleDragLeave() {
    this.setState({
      dragOver: false
    });
    this.props.emit(new CustomEvent('dragLeaveTrash'));
  },
  handleClick() {
    this.props.emit(new CustomEvent('removeTopic'));
  },
  handleDragOver() {
    this.props.emit(new CustomEvent('dragOverTrash'));
  },
  render() {
    let classes = 'trash icon' + (this.state.dragOver ? ' highlight' : '');
    return (
      <i className={classes} onClick={this.handleClick} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver} />
    );
  }
});

var Menu = React.createClass({
  emit(event) {
    this.getDOMNode().dispatchEvent(event);
  },
  render() {
    return (
      <div id="menuContainer">
        <RemoveButton emit={this.emit} />
        <OpenAddModal emit={this.emit} topicTypes={this.props.topicTypes} />
      </div>
    );
  }
});

export default Menu;
