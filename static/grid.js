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
                                  name: $('#nameInput').val(),
                                  capacity: parseInt($('#capacityInput').val())
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
  handleClick: function() {
    var event = new CustomEvent('addBlock',
                                {
                                  'detail': {
                                    'description': $('#descriptionInput').val(),
                                    'range': {
                                      'start':  $('#startInput').val(),
                                      'end'  :  $('#endInput').val()
                                    }
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
                <Input type="text" label="Start" id="startInput"/>
                <Input type="text" label="End" id="endInput"/>
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
  render: function(){
    var ths = this.props.blocks
      .map(function(block){
        return(
            <th key={block.description}>{block.description}</th>
        );
      });
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
            <Tablerow room={room} blocks={blocks} row={_.tail(row)[0]} key={room.name} emit={this.props.emit}></Tablerow>
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
  render: function(){
    var topics = _.zip(this.props.blocks, this.props.row)
          .map(function(zip){
            var block = zip[0];
            var topic = zip[1];
            return(
                <Tablecell key={block.start} room={this.props.room} block={block} topic={topic} emit={this.props.emit} />
            );
      }, this);
    return(
      <tr>
        <td>{this.props.room.name}</td>
        {topics}
      </tr>
    );
  }
});

Tablecell = React.createClass({
  getInitialState: function(){
    return {dragOver: false};
  },
  handleDragStart: function() {
    console.log("dragstart", this.props.topic.value0)
    var event = new CustomEvent('dragStartGridTopic',
                                {'detail': this.props.topic.value0})
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
                                {'detail':{ 'room': this.props.room, 'block': this.props.block }});
    this.props.emit(event);
  },
  render: function(){
    var topic=this.props.topic.value0;
    var highlight = this.state.dragOver;
    return (
      <td>
      <div
        className={highlight ? 'tabletopic highlight' : 'tabletopic'}
        draggable={topic ? "true" : "false"}
        onDragStart={this.handleDragStart}
        onDragEnd={this.handleDragEnd}
        onDragEnter={this.handleDragEnter}
        onDragLeave={this.handleDragLeave}
        onDragOver={this.handleDragOver} >
          {topic ? topic.topic : ''}
      </div>
      </td>
    );
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
