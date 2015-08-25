import React from 'react';
import Topic from './components/Topic.js';

var Topics = React.createClass({
  emit: function (event) {
    this.getDOMNode().dispatchEvent(event);
  },
  render: function () {
    var topics = this.props.topics.map(topic => {
        return <Topic emit={this.emit} key={topic.description} topic={topic}/>;
    });
    return (
      <div id="topicsContainer" className="ui segments">
				<div className="ui segment">
					<h3>Themen</h3>
				</div>
				<div className="ui segment">
          {topics}
				</div>
      </div>
    );
  }
});

export default Topics;
