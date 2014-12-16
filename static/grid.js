var Grid, Tableheader, Tablebody, Tablerow;

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
      var rows = _.zip(this.props.rooms, this.props.grid)
      .map(function(row){
        var room = _.head(row);
        return(
          <Tablerow room={room} row={_.tail(row)[0]} key={room.name}></Tablerow>
        );
      });
      return(
          <tbody>
            {rows}
          </tbody>
      );
    }
});

Tablerow = React.createClass({
  render: function(){
    var topics = this.props.row
    .map(function(topic){
      return(
        <td> {topic.value0 ? topic.value0.description : ''} </td>
      );
    });
    return(
      <tr>
        <td>{this.props.room.name}</td>
        {topics}
      </tr>
    );
  }
});

Grid = React.createClass({
  render: function(){
    return(
      <Table striped bordered condensed>
        <Tableheader blocks={this.props.blocks}></Tableheader>
        <Tablebody rooms={this.props.rooms} grid={this.props.grid}></Tablebody>
      </Table>
    );
  }
});
