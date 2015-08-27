import React, {Component, PropTypes} from 'react';
import {topicTypeColor} from '../utils/topics.js';

export const propTypes = {
  emit: PropTypes.any,
  topic: PropTypes.any,
  room: PropTypes.any,
  block: PropTypes.any
};

export const defaultProps = {
  emit: () => console.log('emit is missing')
};

class Tablecell extends Component {
  constructor() {
    super();
    this.state = { dragOver: false };
    this.handleDragStart = this.handleDragStart.bind(this);
    this.handleDragEnd = this.handleDragEnd.bind(this);
    this.handleDragLeave = this.handleDragLeave.bind(this);
    this.handleDragOver = this.handleDragOver.bind(this);
  }
  handleDragStart(e) {
    e.dataTransfer.setData('text/plain', 'F**k Firefox');
    this.props.emit(
      new CustomEvent('dragStartGridTopic', {
        'detail': this.props.topic.value0
      })
    );
  }
  handleDragEnd() {
    this.props.emit(
      new CustomEvent('dragEndGridTopic', {
        'detail': this.props.topic.value0
      })
    );
  }
  handleDragLeave() {
    this.setState({ dragOver: false });
    this.props.emit(new CustomEvent('dragLeaveSlot'));
  }
  handleDragOver() {
    this.setState({ dragOver: true });
    this.props.emit(
      new CustomEvent('dragOverSlot', {
        'detail': {
          'room': this.props.room,
          'block': this.props.block
        }
      })
    );
  }
  render() {
    const topic = this.props.topic.value0;
    const highlight = this.state.dragOver ? 'highlight ' : '';
    let el;
    if (topic) {
      const classes = 'tabletopic draggable ' + highlight + topicTypeColor(topic.typ);
      el = (
        <td>
          <div
            className={classes} draggable="true" onDragEnd={this.handleDragEnd}
            onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver}
            onDragStart={this.handleDragStart}
            >
						<div className="topic--description">{topic.description}</div>
            <p>Host: <i>{topic.host}</i></p>
          </div>
        </td>
      );
    } else {
      el = (
        <td
          className={highlight} draggable="false" onDragEnd={this.handleDragEnd}
          onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave}
          onDragOver={this.handleDragOver}
        />
      );
    }

    return el;
  }
}

Tablecell.propTypes = propTypes;
Tablecell.defaultProps = defaultProps;

export default Tablecell;
