//module Openspace.Ui.Stream

exports.setDrag = function(){
    document.dispatchEvent(new Event('setDrag'));
};

exports.unsetDrag = function(){
    document.dispatchEvent(new Event('unsetDrag'));
};
