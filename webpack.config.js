var path = require('path');
var webpack = require('webpack');

var DedupePlugin = new webpack.optimize.DedupePlugin();
var UglifyJsPlugin = new webpack.optimize.UglifyJsPlugin();

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
      test: /\.(js|jsx)$/,
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
  // Uncomment for production build
  /* , plugins: [DedupePlugin, UglifyJsPlugin]*/
};

