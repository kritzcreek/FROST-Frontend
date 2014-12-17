var AddModal, OpenAddModal;

var ModalTrigger = ReactBootstrap.ModalTrigger;
var Modal = ReactBootstrap.Modal;
var Button = ReactBootstrap.Button;
var Panel = ReactBootstrap.Panel;
var Input = ReactBootstrap.Input;

AddModal = React.createClass({
  handleClick: function() {
    var event = new CustomEvent('addTopic',
    { 'detail': {
                 description: $('#topicInput').val(),
                 typ: $('#topicTypeInput').val()
                }
    });
    this.props.emit(event);
    this.props.onRequestHide();
  },
  render: function() {
    var options = this.props.topicTypes
    .map(function(topicType){
      return (
        <option key={topicType} value={topicType}>{topicType}</option>
      );
    });
    return (
      <Modal {...this.props} title="Neues Thema" animation={true}>
      <div className="modal-body">
        <form role="form">
          <div className="form-group">
            <label htmlFor="topicInput">Thema: </label>
            <input className="form-control" type="text"   id="topicInput"/>
          </div>
          <div className="form-group">
            <Input type="select" label='Typ'
            defaultValue={_.head(this.props.topicTypes)}
            id="topicTypeInput">
              {options}
            </Input>
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
      <div id="menuContainer">
        <OpenAddModal topicTypes={this.props.topicTypes} emit={this.emit}></OpenAddModal>
        <RemoveButton emit={this.emit}></RemoveButton>
      </div>
    );
  }
});
