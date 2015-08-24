import React from 'react';


var Tablecell = React.createClass({
  getInitialState() {
    return {
      dragOver: false
    };
  },
  handleDragStart(e) {
    e.dataTransfer.setData('text/plain', 'F**k Firefox');
    this.props.emit(
      new CustomEvent('dragStartGridTopic', {
        'detail': this.props.topic.value0
      })
    );
  },
  handleDragEnd() {
    this.props.emit(
      new CustomEvent('dragEndGridTopic', {
        'detail': this.props.topic.value0
      })
    );
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
    this.props.emit(new CustomEvent('dragLeaveSlot'));
  },
  handleDragOver() {
    this.props.emit(
      new CustomEvent('dragOverSlot', {
        'detail': {
          'room': this.props.room,
          'block': this.props.block
        }
      })
    );
  },
  render() {
    var topic = this.props.topic.value0;
    var highlight = this.state.dragOver;
    if (topic) {
      let classes = highlight ? 'tabletopic highlight draggable' : 'tabletopic draggable';
      return (
        <td>
          <div className={classes} draggable="true" onDragEnd={this.handleDragEnd} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver} onDragStart={this.handleDragStart}>
						<p>Host: Zack</p>
						<p>Topic: {topic.description}</p>
          </div>
        </td>
      );
    } else {
      return (
        <td className={highlight ? 'highlight' : ''} draggable="false" onDragEnd={this.handleDragEnd} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver}/>
      );
    }
  }
});

export default Tablecell;
