import React, {Component, PropTypes} from 'react';
import Modal from 'react-modal';

export const propTypes = {
  emit: PropTypes.any,
  topicTypes: PropTypes.any
};

const defaultProps = {
  emit: () => { console.log('emit was missing'); },
  topicTypes: []
};

class NewTopicModal extends Component {
  constructor(props) {
    super(props);
    this.state = { show: false };
    this.open = this.open.bind(this);
    this.close = this.close.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }
  close() {
    this.setState({
      show: false
    });
  }
  open() {
    this.setState({
      show: true
    });
  }
  handleClick() {
    const event = new CustomEvent('addTopic', {
      'detail': {
        description: $('#descriptionInput').val(),
        typ: $('#topicTypeInput').val(),
        host: $('#hostInput').val()
      }
    });
    this.props.emit(event);
    this.close();
  }
  render() {
    const options = this.props.topicTypes.map(topicType =>
      <option key={topicType} value={topicType}>{topicType}</option>
    );
    return (
      <div>
        <button onClick={this.open} className="ui green massive button">
          New Topic
        </button>
        <Modal isOpen={this.state.show} onRequestClose={this.close}>
          <h3 className="ui header">New Topic</h3>
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
            <button type="button" onClick={this.close} className="ui black deny button"> Close </button>
            <button type="button" onClick={this.handleClick} className="ui green button"> Hinzufügen </button>
          </form>
        </Modal>
      </div>
    );
  }
}

NewTopicModal.propTypes = propTypes;
NewTopicModal.defaultProps = defaultProps;

export default NewTopicModal;

