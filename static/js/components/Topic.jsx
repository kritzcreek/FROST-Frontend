import React, {Component, PropTypes} from 'react';
import {topicTypeColor} from '../utils/topics.js';

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
    this.state = { isDragging: false}
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
    this.setState({ isDragging: true });
    const event = new CustomEvent('dragStartTopic', {
      'detail': this.props.topic
    });
    e.dataTransfer.setData('text/plain', 'F**k Firefox');
    this.props.emit(event);
  }
  handleDragEnd() {
    this.setState({ isDragging: false });
    const event = new CustomEvent('dragEndTopic', {
      'detail': this.props.topic
    });
    this.props.emit(event);
  }
  render() {
    const {description, typ, host} = this.props.topic;
    const classes = 'topic draggable ' + topicTypeColor(typ) + (this.state.isDragging ? ' dragging' : '');

    return (
      <div className={classes} draggable="true"
        onClick={this.handleClick} onDragEnd={this.handleDragEnd}
        onDragStart={this.handleDragStart}>
        <div className="topic--description">
          {description}
         </div>
        <div className="typ">
          Typ: {typ}
         </div>
        <div className="typ">
          Host: {host}</div>
      </div>
    );
  }
}

Topic.propTypes = propTypes;
Topic.defaultProps = defaultProps;

export default Topic;
