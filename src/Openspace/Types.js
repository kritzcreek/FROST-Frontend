// module Openspace.Types

exports.parseAssignTopic = function(foreign) {
    return {
        topic: foreign.contents[1],
        slot: foreign.contents[0]
    };
};

exports.serializeAssignTopic = function(slot) {
    return function(topic) {
        return {
            tag: "AssignTopic",
            contents: [
                slot, {
                    description: topic.description,
                    typ: Prelude.show(showTopicType)(topic.typ)
                }
            ]
        };
    };
};
