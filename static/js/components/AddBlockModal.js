import React from 'react';
import Modal from 'react-modal';

var AddBlockModal = React.createClass({
  parseDate(){
    return {
      description: $('#descriptionInput').val(),
      startHours: parseInt($('#startInput').val().slice(0, 2), 10),
      startMinutes: parseInt($('#startInput').val().slice(2, 4), 10),
      endHours: parseInt($('#endInput').val().slice(0, 2), 10),
      endMinutes: parseInt($('#endInput').val().slice(2, 4), 10)
      };
    },
  handleClick() {
    var event = new CustomEvent('addBlock', {
        detail: this.parseDate()
    });
    this.props.emit(event);
    this.props.close();
  },
  render() {
    return (
      <Modal isOpen={this.props.show} onRequestClose={this.props.close}>
        <h3 className="ui header">Add Block</h3>
        <form className="ui form">
          <div className="field">
            <label>Description</label>
            <input id="descriptionInput" type="text"/>
          </div>
          <div className="field" id="start">
            <label>Start</label>
            <input id="startInput" type="number"/>
          </div>
          <div className="field">
            <label>End</label>
            <input id="endInput" type="number"/>
          </div>
          <button type="button" onClick={this.props.close} className="ui button">Close</button>
          <button type="button" onClick={this.handleClick} className="ui button">Add</button>
        </form>
      </Modal>
    );
  }
});

var OpenAddBlockModal = React.createClass({
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
            <AddBlockModal show={this.state.show} close={this.close} emit={this.props.emit}/>
            <button onClick={this.open} className="ui button">New Block</button>
          </div>
        );
    }
});

export default OpenAddBlockModal;
