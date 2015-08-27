import React, { Component, PropTypes } from 'react';
import Modal from 'react-modal';

export const propTypes = {
  emit: PropTypes.any
};

const defaultProps = {
  emit: function emit() { console.info('Missing emit function'); }
};

class NewBlockModal extends Component {
  constructor() {
    super();
    this.state = {show: false};
    this.open = this.open.bind(this);
    this.close = this.close.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  close() {
    this.setState({show: false});
  }

  open() {
    this.setState({show: true});
  }

  parseDate() {
    return {
      description: $('#descriptionInput').val(),
      startHours: parseInt($('#startInput').val().slice(0, 2), 10),
      startMinutes: parseInt($('#startInput').val().slice(2, 4), 10),
      endHours: parseInt($('#endInput').val().slice(0, 2), 10),
      endMinutes: parseInt($('#endInput').val().slice(2, 4), 10)
    };
  }

  handleClick() {
    const event = new CustomEvent('addBlock', {
      detail: this.parseDate()
    });
    this.props.emit(event);
    this.close();
  }

  render() {
    return (
      <div>
        <Modal isOpen={this.state.show} onRequestClose={this.close}>
          <h3 className="ui header">New Block</h3>
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
            <button type="button" onClick={this.close} className="ui button">Cancel</button>
            <button type="button" onClick={this.handleClick} className="ui button">Submit</button>
          </form>
        </Modal>
        <button onClick={this.open} className="ui button">New Block</button>
      </div>
    );
  }
}

NewBlockModal.propTypes = propTypes;
NewBlockModal.defaultProps = defaultProps;

export default NewBlockModal;
