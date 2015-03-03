var Grid, Tableheader, Tablebody, Tablerow, Tablecell;

var Table = ReactBootstrap.Table;
var Button = ReactBootstrap.Button;
var ModalTrigger = ReactBootstrap.ModalTrigger;
var Modal = ReactBootstrap.Modal;
var Input = ReactBootstrap.Input;

AddRoomModal = React.createClass({
  handleClick: function() {
    var event = new CustomEvent('addRoom',
                                {
                                  'detail': {
                                  roomName: $('#nameInput').val(),
                                  roomCapacity: parseInt($('#capacityInput').val())
                                  }
                                });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render: function() {
    return (
        <Modal {...this.props} title="New Room" animation={true}>
          <div className="modal-body">
            <form role="form">
              <div className="form-group">
                <Input type="text" label="Room" id="nameInput"/>
              </div>
              <div className="form-group">
                <Input type="number" label="Capacity" id="capacityInput"> </Input>
              </div>
            </form>
          </div>
          <div className="modal-footer">
            <Button onClick={this.props.onRequestHide}>Close</Button>
            <Button bsStyle="success" onClick={this.handleClick}>Add</Button>
          </div>
        </Modal>
    );
  }
});

OpenAddRoomModal = React.createClass({
  render : function (){
    return (
        <ModalTrigger modal={<AddRoomModal {...this.props}/>}>
          <Button className="btn-block" bsStyle="success">New Room</Button>
        </ModalTrigger>
    );
  }
});

AddBlockModal = React.createClass({
  getInitialState: function() {
    return {
      start: {
        hours: 0, minutes: 0
      },
      end: {
        hours: 0, minutes: 0
      }};
  },
  componentDidMount : function() {
    var self = this
    var start = $('#start');
    start.datetimepicker({
      format: 'LT',
      stepping: 15
    });

    start.on('dp.change', function(event) {
      self.setState(
        {
          start: {
            hours: event.date.hours(),
            minutes: event.date.minutes()
          }
        });
    });

    var end = $('#end');
    end.datetimepicker({
      format: 'LT',
      stepping: 15
    });

    end.on('dp.change', function(event) {
      self.setState(
        {
          end: {
            hours: event.date.hours(),
            minutes: event.date.minutes()
          }
        });
    });

  },
  handleClick: function() {
    var event = new CustomEvent('addBlock',
                                {
                                  'detail': {
                                    'blockDescription' : $('#descriptionInput').val(),
                                    'blockStartHours'  : this.state.start.hours,
                                    'blockStartMinutes': this.state.start.minutes,
                                    'blockEndHours'    : this.state.end.hours,
                                    'blockEndMinutes'  : this.state.end.minutes
                                  }
                                });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render: function() {
    return (
        <Modal {...this.props} title="New Block" animation={true}>
          <div className="modal-body">
            <form role="form">
              <div className="form-group">
                <Input type="text" label="Description" id="descriptionInput"/>
                <label htmlFor="start">Start</label>
                <div className='input-group date' id="start">
                    <Input type='text' className="form-control" />
                    <span className="input-group-addon">
                      <span className="glyphicon glyphicon-time"></span>
                    </span>
                </div>
                <label htmlFor="end">End</label>
                <div className='input-group date' id="end">
                  <Input type='text' className="form-control" />
                  <span className="input-group-addon">
                    <span className="glyphicon glyphicon-time"></span>
                  </span>
                </div>
              </div>
            </form>
          </div>
          <div className="modal-footer">
            <Button onClick={this.props.onRequestHide}>Close</Button>
            <Button bsStyle="success" onClick={this.handleClick}>Add</Button>
          </div>
        </Modal>
    );
  }
});

OpenAddBlockModal = React.createClass({
  render : function (){
    return (
        <ModalTrigger modal={<AddBlockModal {...this.props}/>}>
          <Button className="btn-block" bsStyle="success">New Block</Button>
        </ModalTrigger>
    );
  }
});

Tableheader = React.createClass({
  handleDelete: function(block, event) {
    console.log(block)
    var event = new CustomEvent('deleteBlock', {'detail': block});
    this.props.emit(event);
  },
  render: function(){
    var ths = this.props.blocks
      .map(function(block){
        var timeStart = moment(block.blockStartHours + ':' + block.blockStartMinutes
                               , "HH:mm").format("LT")
        var timeEnd = moment(block.blockEndHours + ':' + block.blockEndMinutes
                             , "HH:mm").format("LT")
        return(
            <th key={block.blockDescription}>
            <div>
              <div>
                {block.blockDescription}
              </div>
              <div>
                {timeStart + " - " + timeEnd}
              </div>
            </div>
            <span className="glyphicon glyphicon-trash"
            onClick={this.handleDelete.bind(this, block)}></span>
            </th>
        );
      }, this);
    return(
      <thead>
        <tr>
        <th></th>
        {ths}
        <th><OpenAddBlockModal emit={this.props.emit}/></th>
      </tr>
    </thead>
    );
  }
});

Tablebody = React.createClass({
  render: function(){
    var blocks= this.props.blocks;
    var rows = _.zip(this.props.rooms, this.props.grid)
      .map(function(row){
        var room = _.head(row);
        return(
            <Tablerow room={room} blocks={blocks} row={_.tail(row)[0]} key={room.roomName} emit={this.props.emit}></Tablerow>
        );
      }, this);
    return(
        <tbody>
        {rows}
        <tr>
          <td>
            <OpenAddRoomModal emit={this.props.emit}/>
          </td>
        </tr>
      </tbody>
    );
  }
});

Tablerow = React.createClass({
  handleDelete : function(e){
    var event = new CustomEvent('deleteRoom', {'detail': this.props.room})
    this.props.emit(event);
  },
  render: function(){
    var topics = _.zip(this.props.blocks, this.props.row)
          .map(function(zip){
            var block = zip[0];
            var topic = zip[1];
            return(
                <Tablecell key={block.start} room={this.props.room}
                 block={block} topic={topic} emit={this.props.emit} />
            );
      }, this);
    return(
      <tr>
        <td>
          {this.props.room.roomName}
          <span className="glyphicon glyphicon-trash"
          onClick={this.handleDelete}>
          </span>
        </td>
        {topics}
      </tr>
    );
  }
});

Tablecell = React.createClass({
  getInitialState: function(){
    return {dragOver: false};
  },
  handleDragStart: function(e) {
    var event = new CustomEvent('dragStartGridTopic',
                                {'detail': this.props.topic.value0})
    e.dataTransfer.setData("text/html", "F**k Firefox");
    this.props.emit(event);
  },
  handleDragEnd: function(e) {
    var event = new CustomEvent('dragEndGridTopic',
                                {'detail': this.props.topic.value0});
    this.props.emit(event);
  },
  handleDragEnter : function(e){
    this.setState({dragOver: true});
  },
  handleDragLeave : function(e){
    this.setState({dragOver: false});
    var event = new CustomEvent('dragLeaveSlot');
    this.props.emit(event);
  },
  handleDragOver: function(e){
    var event = new CustomEvent('dragOverSlot',
                                {'detail':{ 'room': this.props.room,
                                            'block': this.props.block }});
    this.props.emit(event);
  },
  render: function(){
    var topic=this.props.topic.value0;
    var highlight = this.state.dragOver;
    if(topic){
      return (
        <td>
        <div
        className={highlight ? 'tabletopic highlight draggable' : 'tabletopic draggable'}
        draggable={'true'}
        onDragStart={this.handleDragStart}
        onDragEnd={this.handleDragEnd}
        onDragEnter={this.handleDragEnter}
        onDragLeave={this.handleDragLeave}
        onDragOver={this.handleDragOver} >
          {topic.topicDescription}
        </div>
        </td>
      );
    }else{
    return (
      <td
        className={highlight ? 'highlight' : ''}
        draggable={'false'}
        onDragEnd={this.handleDragEnd}
        onDragEnter={this.handleDragEnter}
        onDragLeave={this.handleDragLeave}
        onDragOver={this.handleDragOver}
      />
    );
    }
  }
});

Grid = React.createClass({
  emit: function(event){
    this.getDOMNode().dispatchEvent(event);
  },
  render: function(){
    return(
        <Table striped bordered condensed id='gridContainer'>
          <Tableheader blocks={this.props.blocks} emit={this.emit}/>
          <Tablebody {...this.props} emit={this.emit}/>
        </Table>
    );
  }
});
