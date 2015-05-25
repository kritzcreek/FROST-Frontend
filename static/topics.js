import {
  Panel
}
from 'react-bootstrap';
import React from 'react';

var Topic = React.createClass({
  handleClick: function () {
    var event = new CustomEvent('clickTopic', {
      'detail': this.props.topic
    });
    this.props.emit(event);
  },
  handleDragStart: function (e) {
    var event = new CustomEvent('dragStartTopic', {
      'detail': this.props.topic
    });
    e.dataTransfer.setData('text/plain', 'F**k Firefox');
    this.props.emit(event);
  },
  handleDragEnd: function () {
    var event = new CustomEvent('dragEndTopic', {
      'detail': this.props.topic
    });
    this.props.emit(event);
  },
  render: function () {
    return (
      <div className={"topic draggable"} draggable="true"
        onClick={this.handleClick} onDragEnd={this.handleDragEnd}
        onDragStart={this.handleDragStart}>
        <div className="description">
          Thema: {this.props.topic.description}</div>
        <div className="typ">
          Typ: {this.props.topic.typ}</div>
      </div>
    );
  }
});

var Topics = React.createClass({
  emit: function (event) {
    this.getDOMNode().dispatchEvent(event);
  },
  render: function () {
    var topics = this.props.topics.map(function (topic) {
      return (
        <Topic emit={this.emit} key={topic.description} topic={topic}/>
      );
    }, this);
    return (
      <Panel bsStyle="primary" header="Themen" id="topicsContainer">
        <div id="topic">
          {topics}
        </div>
      </Panel>
    );
  }
});

export
default Topics;
