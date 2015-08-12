// module Openspace.Ui.Render

exports.renderMenu = function(topicTypes) {
    return function() {
        React.render(
            React.createElement(Menu, {
                topicTypes: topicTypes
            }),
            document.getElementById('menu')
        );
    };
};

exports.renderTopicsImpl = function(topics) {
    return function() {
        React.render(
            React.createElement(Topics, {
                topics: topics
            }),
            document.getElementById('topics')
        );
    };
};

exports.renderGridImpl = function(rooms, blocks, grid) {
    return function() {
        React.render(
            React.createElement(Grid, {
                rooms: rooms,
                blocks: blocks,
                grid: grid
            }),
            document.getElementById('grid')
        );
    };
};
