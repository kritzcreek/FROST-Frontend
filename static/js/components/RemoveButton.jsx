import React from 'react';

var RemoveButton = React.createClass({
    getInitialState() {
        return {
            dragOver: false
        };
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
        this.props.emit(new CustomEvent('dragLeaveTrash'));
    },
    handleClick() {
        this.props.emit(new CustomEvent('removeTopic'));
    },
    handleDragOver() {
        this.props.emit(new CustomEvent('dragOverTrash'));
    },
    render() {
        let classes = 'ui big-button button' + (this.state.dragOver ? ' highlight-alt' : '');
       return (
         <div>
           <div className={classes} onClick={this.handleClick} onDragEnter={this.handleDragEnter} onDragLeave={this.handleDragLeave} onDragOver={this.handleDragOver}>
            Remove Topic
            </div>
          </div>
        );
    }
});

export default RemoveButton;
