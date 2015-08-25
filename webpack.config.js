var path = require('path');

module.exports = {
    entry: './static/js/entry.js',
    output: {
        path: path.join(__dirname, 'dist'),
        filename: 'js/app.js'
    },
    module: {
        loaders: [{
            test: /\.purs$/,
            loader: 'purs',
            query: {
                output: 'output'
            }
        }, {
            test: /\.js$/,
            exclude: /node_modules/,
            loader: 'babel'
        }]
    },
    resolve: {
        modulesDirectories: [
            'node_modules',
            'web_modules',
            'output'
        ]
    }
};
