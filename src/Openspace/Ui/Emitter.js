// module Openspace.Ui.Emitter

exports.onAsObservable = function(str){
    return function(el){
        return function(){
            return el.onAsObservable(str);
        };
    };
};
