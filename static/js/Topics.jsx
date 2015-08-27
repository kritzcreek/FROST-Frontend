import React, {Component, PropTypes} from 'react';
import Topic from './components/Topic.jsx';

export const propTypes = {
  topics: PropTypes.any
};

class Topics extends Component {
  emit(event) {
    React.findDOMNode(this).dispatchEvent(event);
  }
  render() {
    const topics = this.props.topics.map(topic =>
      <Topic emit={this.emit.bind(this)} key={topic.description} topic={topic}/>
    );
    const header = (<div className="ui segment"><h2>Drag your topic onto the time table</h2></div>);
    return (
      <div id="topicsContainer" className="ui segments">
        { topics.length !== 0 ? header : null }
        <div className="ui segment">
          {topics}
        </div>
      </div>
    );
  }
}

Topics.propTypes = propTypes;

export default Topics;
