import React from 'react';
import {
  Table,
  Button,
  ModalTrigger,
  Modal,
  Input
}
from 'react-bootstrap';
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
    this.props.onRequestHide();
  },
  render() {
    return (
      <Modal animation={true} title="New Room" {...this.props}>
        <div className="modal-body">
          <form role="form">
            <div className="form-group">
              <Input id="nameInput" label="Room" type="text"/>
            </div>
            <div className="form-group">
              <Input id="capacityInput" label="Capacity" type="number"></Input>
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

var OpenAddRoomModal = React.createClass({
  render() {
    return (
      <ModalTrigger modal={<AddRoomModal {...this.props} />} >
        <Button bsStyle="success" className="btn-block">New Room</Button>
      </ModalTrigger>
    );
  }
});

var AddBlockModal = React.createClass({
  getInitialState() {
    return {
      start: {
        hours: 0,
        minutes: 0
      },
      end: {
        hours: 0,
        minutes: 0
      }
    };
  },
  componentDidMount() {
    var self = this;
    var start = $('#start');
    start.datetimepicker({
      format: 'LT',
      stepping: 15
    });

    start.on('dp.change', function (event) {
      self.setState({
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

    end.on('dp.change', function (event) {
      self.setState({
        end: {
          hours: event.date.hours(),
          minutes: event.date.minutes()
        }
      });
    });

  },
  handleClick() {
    var event = new CustomEvent('addBlock', {
      'detail': {
        'description': $('#descriptionInput').val(),
        'startHours': this.state.start.hours,
        'startMinutes': this.state.start.minutes,
        'endHours': this.state.end.hours,
        'endMinutes': this.state.end.minutes
      }
    });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render() {
    return (
      <Modal animation={true} title="New Block" {...this.props}>
        <div className="modal-body">
          <form role="form">
            <div className="form-group">
              <Input id="descriptionInput" label="Description" type="text"/>
              <label htmlFor="start">Start</label>
              <div className='input-group date' id="start">
                <Input className="form-control" type='text'/>
                <span className="input-group-addon">
                  <span className="glyphicon glyphicon-time"></span>
                </span>
              </div>
              <label htmlFor="end">End</label>
              <div className='input-group date' id="end">
                <Input className="form-control" type='text'/>
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

var OpenAddBlockModal = React.createClass({
  render() {
    return (
      <ModalTrigger modal={ <AddBlockModal {...this.props} />} >
        <Button bsStyle="success" className="btn-block">New Block</Button>
      </ModalTrigger>
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
          <div>
            <div>
              {block.description}
            </div>
            <div>
              {timeStart + ' - ' + timeEnd}
            </div>
          </div>
          <span className="glyphicon glyphicon-trash" onClick={this.handleDelete.bind(this, block)} />
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
          <span className="glyphicon glyphicon-trash" onClick={this.handleDelete} />
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
      <Table bordered condensed id='gridContainer' striped>
        <Tableheader blocks={this.props.blocks} emit={this.emit}/>
        <Tablebody emit={this.emit} {...this.props}/>
      </Table>
    );
  }
});

export default Grid;
