import {Button, Panel, ModalTrigger, Modal, Input} from 'react-bootstrap'
import React from 'react'
import _ from 'lodash'

var AddModal = React.createClass({
  handleClick: function() {
    var event = new CustomEvent('addTopic',
                                {
                                  'detail': {
                                  description: $('#descriptionInput').val(),
                                  typ: $('#topicTypeInput').val()
                                  }
                                });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render: function() {
    var options = this.props.topicTypes
          .map(function(topicType){
            return (
                <option key={topicType} value={topicType}>{topicType}</option>
            );
          });
    return (
        <Modal {...this.props} title="Neues Thema" animation={true}>
          <div className="modal-body">
            <form role="form">
              <div className="form-group">
                <label htmlFor="descriptionInput">Thema: </label>
                <input className="form-control" type="text" id="descriptionInput"/>
              </div>
              <div className="form-group">
                <Input type="select" label='Typ'
                  defaultValue={_.head(this.props.topicTypes)}
                  id="topicTypeInput">
                  {options}
                </Input>
              </div>
            </form>
        </div>
        <div className="modal-footer">
          <Button onClick={this.props.onRequestHide}>Close</Button>
          <Button bsStyle="success" onClick={this.handleClick}> Hinzufügen </Button>
        </div>
        </Modal>
    );
  }
});

var OpenAddModal = React.createClass({
  render: function (){
    return (
        <ModalTrigger modal={<AddModal {...this.props}/>}>
          <span className={"glyphicon glyphicon-plus"}>
          </span>
        </ModalTrigger>
    );
  }
});

var RemoveButton = React.createClass({
  getInitialState: function(){
    return {dragOver: false};
  },
  handleDragEnter : function(){
    this.setState({dragOver: true});
  },
  handleDragLeave : function(){
    this.setState({dragOver: false});
    var event = new CustomEvent('dragLeaveTrash');
    this.props.emit(event);
  },
  handleClick: function() {
    var event = new CustomEvent('removeTopic');
    this.props.emit(event);
  },
  handleDragOver: function() {
    var event = new CustomEvent('dragOverTrash');
    this.props.emit(event);
  },
  render: function(){
    return (
      <span
      className={'glyphicon glyphicon-trash' + (this.state.dragOver? ' highlight' : '')}
      onClick={this.handleClick}
      onDragEnter={this.handleDragEnter}
      onDragLeave={this.handleDragLeave}
      onDragOver={this.handleDragOver}>
      </span>
    );
  }
});

var Menu = React.createClass({
  emit: function(event){
    this.getDOMNode().dispatchEvent(event);
  },
  render: function(){
    return (
        <div id="menuContainer">
          <span className="notice">
            <span> Drop a Topic </span>
            <span className="glyphicon glyphicon-arrow-right"/></span>
            <RemoveButton emit={this.emit}></RemoveButton>
            <OpenAddModal topicTypes={this.props.topicTypes} emit={this.emit}></OpenAddModal>
            <span className="notice">
              <span className="glyphicon glyphicon-arrow-left"/>
              <span>Click to add a Topic</span>
          </span>
        </div>
    );
  }
});

export default Menu
