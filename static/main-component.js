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
  render: function() {
    return (
      <div className="timeslot">
        <RoomTime values={this.props.tuple.value0}></RoomTime>
        <Topic values={this.props.tuple.value1}></Topic>
      </div>
    );
  }
});

MainApp = React.createClass({
  render: function() {
    var timedTopics = this.props.appState.timeslots
      .map(function(timeslot){
        return (
          <TimeSlot tuple={timeslot}> </TimeSlot>
        );
      },this);
    return (
      <div className="app">
        {timedTopics}
      </div>
    );
  }
});
