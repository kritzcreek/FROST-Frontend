import React from 'react';
import Modal from 'react-modal';


var NewRoomModal = React.createClass({
  handleClick() {
    var event = new CustomEvent('addRoom', {
      'detail': {
        name: $('#nameInput').val(),
        capacity: parseInt($('#capacityInput').val())
      }
    });
      this.props.emit(event);
      this.props.close();
  },
  render() {
    return (
      <Modal isOpen={this.props.show} onRequestClose={this.props.close}>
        <h3 className="ui header">New Room</h3>
        <form className="ui form" role="form">
          <div className="field">
            <label>Name</label>
            <input id="nameInput" type="text"/>
          </div>
          <div className="field">
            <label>Capacity</label>
            <input id="capacityInput" type="number"/>
          </div>
          <button type="button" onClick={this.props.close} className="ui button">Cancel</button>
          <button type="button" onClick={this.handleClick} className="ui button">Submit</button>
        </form>
      </Modal>
    );
  }
});


var OpenNewRoomModal = React.createClass({
    getInitialState(){
        return {show: false};
    },
    open(){
        this.setState({show: true});
    },
    close(){
        this.setState({show: false});
    },
    render() {
        return (
          <div>
            <NewRoomModal show={this.state.show} close={this.close} emit={this.props.emit}/>
            <button onClick={this.open} className="ui button">New Room</button>
          </div>
        );
    }
});

export default OpenNewRoomModal;
