var AddModal, OpenAddModal;

var ModalTrigger = ReactBootstrap.ModalTrigger;
var Modal = ReactBootstrap.Modal;
var Button = ReactBootstrap.Button;
var Panel = ReactBootstrap.Panel;

AddModal = React.createClass({
  handleClick: function() {
    var event = new CustomEvent('addTopic',
    { 'detail': {
                 description: $('#topicInput').val(),
                 typ: $('#typInput').val()
                }
    });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render: function() {
    return (
      <Modal {...this.props} title="Neues Thema" animation={true}>
      <div className="modal-body">
        <form role="form">
          <div className="form-group">
            <label htmlFor="topicInput">Thema: </label>
            <input className="form-control" type="text"   id="topicInput"/>
          </div>
          <div className="form-group">
            <label htmlFor="typInput">Art: </label>
            <input className="form-control" type="text"   id="typInput"/>
          </div>
        </form>
      </div>
      <div className="modal-footer">
        <Button onClick={this.props.onRequestHide}>Close</Button>
        <Button bsStyle="success" onClick={this.handleClick}> Hinzufügen </Button>
      </div>
      </Modal>
    );
  }
});

OpenAddModal = React.createClass({
  render : function (){
    return (
      <ModalTrigger modal={<AddModal {...this.props}/>}>
        <Button className="modalTrigger" bsStyle="primary" bsSize="large">Neues Thema</Button>
      </ModalTrigger>
    );
  }
});

RemoveButton = React.createClass({
  handleClick: function() {
    var event = new CustomEvent('removeTopic');
    this.props.emit(event);
  },
  render: function(){
    return (
      <Button className="removeButton" bsStyle="danger"
      bsSize="large" onClick={this.handleClick}>
        Thema löschen
      </Button>
    );
  }
});

Menu = React.createClass({
  emit : function(event){
    this.getDOMNode().dispatchEvent(event);
  },
  render: function(){
    return (
      <Panel id="menuContainer" header="Menü" bsStyle="primary">
        <OpenAddModal emit={this.emit}></OpenAddModal>
        <RemoveButton emit={this.emit}></RemoveButton>
      </Panel>
    );
  }
});


React.render(
  React.createElement(Menu, null),
  document.getElementById('menu')
);
