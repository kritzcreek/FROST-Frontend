import React, {Component, PropTypes} from 'react';
import Modal from 'react-modal';

export const propTypes = {
  emit: PropTypes.any
};

const defaultProps = {
  emit: function emit() { console.info('Missing emit function'); }
};

class NewRoomModal extends Component {
  constructor(props) {
    super(props);
    this.state = {show: false};
    this.open = this.open.bind(this);
    this.close = this.close.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  open() {
    this.setState({
      show: true
    });
  }

  close() {
    this.setState({
      show: false
    });
  }

  handleClick() {
    const event = new CustomEvent('addRoom', {
      'detail': {
        name: $('#nameInput').val(),
        capacity: parseInt($('#capacityInput').val(), 10)
      }
    });
    this.props.emit(event);
    this.close();
  }

  render() {
    return (
      <div className="inline-button">
        <button onClick={this.open} className="ui huge button">New Room</button>
        <Modal isOpen={this.state.show} onRequestClose={this.close}>
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
            <button type="button" onClick={this.close} className="ui button">Cancel</button>
            <button type="button" onClick={this.handleClick} className="ui button">Submit</button>
          </form>
        </Modal>
      </div>
    );
  }
}

NewRoomModal.propTypes = propTypes;
NewRoomModal.defaultProps = defaultProps;

export default NewRoomModal;
