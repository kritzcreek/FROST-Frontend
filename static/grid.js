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
    return {mouseOver:false, selected:false};
  },
  handleMouseOver: function(e){
    this.setState({mouseOver : true});
  },
  handleMouseOut:function(e){
    this.setState({mouseOver : false});
  },
  handleDragOver: function(e){
    var event = new CustomEvent('dragOverSlot',
				{'detail':{ 'room': this.props.room, 'block': this.props.block }});
    this.props.emit(event);
  },
  handleClick: function(e){
    var topic=this.props.topic.value0;
    var event;
    if(topic){
      event = new CustomEvent('selectSlotWithTopic',
                              { 'detail': {'description': topic.description, 'typ' :topic.typ}});
    }else{
      event = new CustomEvent('selectSlotWithoutTopic',
                              { 'detail': { 'room': this.props.room, 'block': this.props.block}});
      this.setState({selected : !this.state.selected});
    }
    this.props.emit(event);
  },
  render: function(){
    var topic=this.props.topic.value0;
    var highlight = !topic && (this.state.mouseOver || this.state.selected);
    return (
        <td
      className={highlight ? 'highlight' : ''}
      onMouseOver={this.handleMouseOver}
      onMouseOut={this.handleMouseOut}
      onClick={this.handleClick}
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
