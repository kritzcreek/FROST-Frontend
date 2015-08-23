import React from 'react';
import Modal from 'react-modal';
import moment from 'moment';
import _ from 'lodash';

var AddRoomModal = React.createClass({
  handleClick() {
    var event = new CustomEvent('addRoom', {
      'detail': {
        name: $('#nameInput').val(),
        capacity: parseInt($('#capacityInput').val())
      }
    });
      this.props.emit(event);
      this.props.close();
  },
  render() {
    return (
      <Modal isOpen={this.props.show} onRequestClose={this.props.close}>
        <h3 className="ui header">Add Room</h3>
        <form className="ui form" role="form">
          <div className="field">
            <label>Name</label>
            <input id="nameInput" type="text"/>
          </div>
          <div className="field">
            <label>Capacity</label>
            <input id="capacityInput" type="number"/>
          </div>
          <button type="button" onClick={this.props.close} className="ui black deny button">
            Cancel
          </button>
          <button type="button" onClick={this.handleClick} className="ui button">Add</button>
        </form>
      </Modal>
    );
  }
});


var OpenAddRoomModal = React.createClass({
    getInitialState(){
        return {show: false};
    },
    open(){
        this.setState({show: true});
    },
    close(){
        this.setState({show: false});
    },
    render() {
        return (
          <div>
            <AddRoomModal show={this.state.show} close={this.close} emit={this.props.emit}/>
            <button onClick={this.open} className="ui button">Add Room</button>
          </div>
        );
    }
});

var AddBlockModal = React.createClass({
  parseDate(){
    return {
      description: $('#descriptionInput').val(),
      startHours: parseInt($('#startInput').val().slice(0, 2), 10),
      startMinutes: parseInt($('#startInput').val().slice(2, 4), 10),
      endHours: parseInt($('#endInput').val().slice(0, 2), 10),
      endMinutes: parseInt($('#endInput').val().slice(2, 4), 10)
      };
    },
  handleClick() {
    var event = new CustomEvent('addBlock', {
        detail: this.parseDate()
    });
    this.props.emit(event);
    this.props.close();
  },
  render() {
    return (
      <Modal isOpen={this.props.show} onRequestClose={this.props.close}>
        <h3 className="ui header">Add Block</h3>
        <form className="ui form">
          <div className="field">
            <label>Description</label>
            <input id="descriptionInput" type="text"/>
          </div>
          <div className="field" id="start">
            <label>Start</label>
            <input id="startInput" type="number"/>
          </div>
          <div className="field">
            <label>End</label>
            <input id="endInput" type="number"/>
          </div>
          <button type="button" onClick={this.props.close} className="ui button">Close</button>
          <button type="button" onClick={this.handleClick} className="ui button">Add</button>
        </form>
      </Modal>
    );
  }
});

var OpenAddBlockModal = React.createClass({
    getInitialState(){
        return { show: false };
    },
    close(){
        this.setState({show: false});
    },
    open(){
        this.setState({show: true});
    },
    render() {
        return (
          <div>
            <AddBlockModal show={this.state.show} close={this.close} emit={this.props.emit}/>
            <button onClick={this.open} className="ui button">New Block</button>
          </div>
        );
    }
});

var Tableheader = React.createClass({
  handleDelete(block) {
    var event = new CustomEvent('deleteBlock', {
      'detail': block
    });
    this.props.emit(event);
  },
  render() {
    var ths = this.props.blocks.map(block => {
      var timeStart = moment(block.startHours + ':' + block.startMinutes, 'HH:mm').format('LT');
      var timeEnd = moment(block.endHours + ':' + block.endMinutes, 'HH:mm').format('LT');
      return (
        <th key={block.description}>
          <i className="close icon" onClick={this.handleDelete.bind(this, block)} />
          <div>
            <div>
              {block.description}
            </div>
            <div>
              {timeStart + ' - ' + timeEnd}
            </div>
          </div>
        </th>
      );
    });
    return (
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

var Tablebody = React.createClass({
  render() {
    var blocks = this.props.blocks;
    var rows = _.zip(this.props.rooms, this.props.grid).map(row => {
      var room = _.head(row);
      return (
        <Tablerow blocks={blocks} emit={this.props.emit} key={room.name} room={room} row={_.tail(row)[0]} />
      );
    });
    return (
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

var Tablerow = React.createClass({
  handleDelete() {
    this.props.emit(
      new CustomEvent('deleteRoom', {
        'detail': this.props.room
      })
    );
  },
  render() {
    var topics = _.zip(this.props.blocks, this.props.row)
    .map(([block, topic]) =>
        <Tablecell block={block} emit={this.props.emit} key={block.start} room={this.props.room} topic={topic} />
    );
    return (
      <tr>
        <td>
          {this.props.room.name}
          <i className="close icon" onClick={this.handleDelete} />
        </td>
          {topics}
      </tr>
    );
  }
});

var Tablecell = React.createClass({
  getInitialState() {
    return {
      dragOver: false
    };
  },
  handleDragStart(e) {
    e.dataTransfer.setData('text/plain', 'F**k Firefox');
    this.props.emit(
      new CustomEvent('dragStartGridTopic', {
        'detail': this.props.topic.value0
      })
    );
  },
  handleDragEnd() {
    this.props.emit(
      new CustomEvent('dragEndGridTopic', {
        'detail': this.props.topic.value0
      })
    );
  },
  handleDragEnter() {
    this.setState({
      dragOver: true
    });
  },
  handleDragLeave() {
    this.setState({
      dragOver: false
    });
    this.props.emit(new CustomEvent('dragLeaveSlot'));
  },
  handleDragOver() {
    this.props.emit(
      new CustomEvent('dragOverSlot', {
        'detail': {
          'room': this.props.room,
          'block': this.props.block
        }
      })
    );
  },
  render() {
    var topic = this.props.topic.value0;
    var highlight = this.state.dragOver;
    if (topic) {
      let classes = highlight ? 'tabletopic highlight draggable' : 'tabletopic draggable';
      return (
        <td>
          <div className={classes} draggable="true" onDragEnd={this.handleDragEnd} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver} onDragStart={this.handleDragStart}>
            {topic.description}
          </div>
        </td>
      );
    } else {
      return (
        <td className={highlight ? 'highlight' : ''} draggable="false" onDragEnd={this.handleDragEnd} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver}/>
      );
    }
  }
});

var Grid = React.createClass({
  emit(event) {
    this.getDOMNode().dispatchEvent(event);
  },
  render() {
    return (
      <table id='gridContainer' className="ui celled striped table">
        <Tableheader blocks={this.props.blocks} emit={this.emit}/>
        <Tablebody emit={this.emit} {...this.props}/>
      </table>
    );
  }
});

export default Grid;
