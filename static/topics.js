var Topics, Topic;

var Panel = ReactBootstrap.Panel;

Topic = React.createClass({
  handleClick: function(e) {
    var event = new CustomEvent('selectTopic',
    { 'detail': this.props.values });
    this.props.emit(event);
  },
  render: function(){
    return (
      <div className={this.props.selected ? "topic selected" : "topic"} onClick={this.handleClick}>
        <div className="description"> Thema: {this.props.values.description}</div>
        <div className="typ"> Typ: {this.props.values.typ}</div>
      </div>
    );
  }
});

Topics = React.createClass({
  emit : function(event){
    this.getDOMNode().dispatchEvent(event);
  },
  render: function(){
    var _selected = this.props.topicsAndSelected.value1;
    var topics = this.props.topicsAndSelected.value0
    .map(function(topic){
      var selected = topic.description === (_selected.value0 && _selected.value0.description);
      return (
        <Topic values={topic} key={topic.description} emit={this.emit}
        selected={selected}> </Topic>
      );
    },this);
    return (
      <Panel id="topicsContainer" header="Themen" bsStyle="primary">
      <div id="topic">
      {topics}
      </div>
      </Panel>
    );
  }
});
