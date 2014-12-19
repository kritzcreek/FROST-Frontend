var Grid, Tableheader, Tablebody, Tablerow, Tablecell;

var Table = ReactBootstrap.Table;

Tableheader = React.createClass({
  render: function(){
    var ths = this.props.blocks
      .map(function(block){
	return(
            <th key={block.start}>{block.start}</th>
	);
      });
    return(
	<thead>
        <tr>
        <th></th>
        {ths}
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
    var highlight = !topic && this.state.dragOver;
    return (
        <td
      className={highlight ? 'highlight' : ''}
      onDragEnter={this.handleDragEnter}
      onDragLeave={this.handleDragLeave}
      onDragOver={this.handleDragOver} >
	{topic ? topic.description : ''}
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
        <Tableheader blocks={this.props.blocks} />
        <Tablebody {...this.props} emit={this.emit}/>
	</Table>
    );
  }
});
