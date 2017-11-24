var path = require('path');
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');

// Phaser webpack config
var phaserModule = path.join(__dirname, '/node_modules/phaser/');
var phaser = path.join(phaserModule, 'build/custom/phaser-split.js');
var pixi = path.join(phaserModule, 'build/custom/pixi.js');
var p2 = path.join(phaserModule, 'build/custom/p2.js');

module.exports = {
  entry: path.join(__dirname, 'src/app.ts'),
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'app.js',
    publicPath: '/'
  },
  plugins: [
    new CopyWebpackPlugin([
      {
        from: './src/assets',
        to:'./assets'
      }
    ]),
    new HtmlWebpackPlugin({
      template: './index.html',
      inject: 'body',
    }),
    new webpack.NoEmitOnErrorsPlugin(),
  ],
  module: {
    loaders: [
      { test: /pixi\.js/, loader: 'expose-loader?PIXI' },
      { test: /phaser-split\.js$/, loader: 'expose-loader?Phaser' },
      { test: /p2\.js/, loader: 'expose-loader?p2' },
      { test: /\.ts$/, loader: 'ts-loader', exclude: '/node_modules/' }
    ]
  },
  node: {
    fs: 'empty'
  },
  resolve: {
    extensions: ['.ts', '.js'],
    alias: {
        pixi: path.join(__dirname, 'node_modules/phaser-ce/build/custom/pixi.js'),
        phaser: path.join(__dirname, 'node_modules/phaser-ce/build/custom/phaser-split.js'),
        p2: path.join(__dirname, 'node_modules/phaser-ce/build/custom/p2.js')
    }
},
  devtool: 'source-map'
}