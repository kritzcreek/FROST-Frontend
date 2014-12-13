var RoomTime, Topic, Room, MainApp;

RoomTime = React.createClass({
  render : function(){
    return (
      <div className="roomTime">
        <div className="room"> Raum: {this.props.values.room}</div>
        <div className="time"> Zeit: {this.props.values.time}</div>
      </div>
    );
  }
});

Topic = React.createClass({
  render: function(){
    return (
      <div className="topic">
      <div className="description"> Thema: {this.props.values.description}</div>
      <div className="typ"> Typ: {this.props.values.typ}</div>
      </div>
    );
  }
});

TimeSlot = React.createClass({
  handleClick : function(e){
    var event = new CustomEvent('selectTimeSlot',
                  { 'detail': this.props.tuple.value1 });
    this.props.emit(event);
  },

  render: function() {
    return (
      <div className={this.props.selected ? 'timeslot selected' : ' ' + ' timeslot'} onClick={this.handleClick}>
        <RoomTime values={this.props.tuple.value0}></RoomTime>
        <Topic values={this.props.tuple.value1}></Topic>
      </div>
    );
  }
});

MainApp = React.createClass({
  emit : function(event){
    this.getDOMNode().dispatchEvent(event);
  },

  render: function() {
    var timedTopics = this.props.appState.timeslots
      .map(function(timeslot){
        var selected = timeslot.value1.description === this.props.appState.selected.value0.description;
        return (
          <TimeSlot
          selected={selected}
          key={timeslot.value1.description}
          tuple={timeslot} emit={this.emit}> </TimeSlot>
        );
      },this);
    return (
      <div className="app">
        {timedTopics}
      </div>
    );
  }
});
