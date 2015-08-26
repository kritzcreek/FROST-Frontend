import React, {Component, PropTypes} from 'react';

export const propTypes = {
  emit: PropTypes.any,
  topic: PropTypes.any
};

const defaultProps = {
  emit: () => console.log('emit was missing'),
  topic: null
};

class Topic extends Component {
  constructor() {
    super();
    this.handleClick = this.handleClick.bind(this);
    this.handleDragStart = this.handleDragStart.bind(this);
    this.handleDragEnd = this.handleDragEnd.bind(this);
  }
  handleClick() {
    const event = new CustomEvent('clickTopic', {
      'detail': this.props.topic
    });
    this.props.emit(event);
  }
  handleDragStart(e) {
    const event = new CustomEvent('dragStartTopic', {
      'detail': this.props.topic
    });
    e.dataTransfer.setData('text/plain', 'F**k Firefox');
    this.props.emit(event);
  }
  handleDragEnd() {
    const event = new CustomEvent('dragEndTopic', {
      'dconst': this.props.topic
    });
    this.props.emit(event);
  }
  render() {
    const {description, typ, host} = this.props.topic;
    return (
      <div className={"topic draggable"} draggable="true"
        onClick={this.handleClick} onDragEnd={this.handleDragEnd}
        onDragStart={this.handleDragStart}>
        <div className="description">
          Thema: {description}</div>
        <div className="typ">
          Typ: {typ}</div>
        <div className="typ">
          Host: {host}</div>
      </div>
    );
  }
}

Topic.propTypes = propTypes;
Topic.defaultProps = defaultProps;

export default Topic;
