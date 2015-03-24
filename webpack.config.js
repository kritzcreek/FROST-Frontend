var path = require('path');

module.exports = {
  entry: './static/entry.js',
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'js/app.js'
  },
  loaders: [
    { test: /\.purs$/,
      loader: 'purs',
      query: { output: 'output' } },
    { test: /\.js$/,
      exclude: /node_modules/,
      loader: 'babel-loader'}
  ],
  resolve: {
    modulesDirectories: [
      'node_modules',
      'web_modules',
      'output'
    ]
  }
};