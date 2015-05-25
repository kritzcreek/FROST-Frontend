import {
  Button,
  Panel,
  ModalTrigger,
  Modal,
  Input
}
from 'react-bootstrap';
import React from 'react';
import _ from 'lodash';

var AddModal = React.createClass({
  handleClick() {
    var event = new CustomEvent('addTopic', {
      'detail': {
        description: $('#descriptionInput').val(),
        typ: $('#topicTypeInput').val()
      }
    });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render() {
    var options = this.props.topicTypes.map(topicType =>
        <option key={topicType} value={topicType}>{topicType}</option>
    );
    return (
      <Modal animation={true} title="Neues Thema" {...this.props}>
        <div className="modal-body">
          <form role="form">
            <div className="form-group">
              <label htmlFor="descriptionInput">Thema:
              </label>
              <input className="form-control" id="descriptionInput" type="text"/>
            </div>
            <div className="form-group">
              <Input defaultValue={_.head(this.props.topicTypes)} id="topicTypeInput" label='Typ' type="select">
                {options}
              </Input>
            </div>
          </form>
        </div>
        <div className="modal-footer">
          <Button onClick={this.props.onRequestHide}>Close</Button>
          <Button bsStyle="success" onClick={this.handleClick}>
            Hinzufügen
          </Button>
        </div>
      </Modal>
    );
  }
});

var OpenAddModal = React.createClass({
  render() {
    return (
      <ModalTrigger modal={ <AddModal {...this.props}/>}>
        <span className={"glyphicon glyphicon-plus"} />
      </ModalTrigger>
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
    let classes = 'glyphicon glyphicon-trash' + (this.state.dragOver ? ' highlight' : '');
    return (
      <span className={classes} onClick={this.handleClick} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver} />
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
        <span className="notice">
          <span>
            Drop a Topic
          </span>
          <span className="glyphicon glyphicon-arrow-right"/>
        </span>
        <RemoveButton emit={this.emit} />
        <OpenAddModal emit={this.emit} topicTypes={this.props.topicTypes} />
        <span className="notice">
          <span className="glyphicon glyphicon-arrow-left" />
          <span>Click to add a Topic</span>
        </span>
      </div>
    );
  }
});

export default Menu;
