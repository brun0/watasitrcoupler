const path = require('path');
const webpack = require('webpack');
const { VueLoaderPlugin } = require('vue-loader');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');

module.exports = {
	entry: {
		script: path.join(__dirname, 'src', 'src.js'),
	},
	output: {
		path: path.join(__dirname, 'js'),
		publicPath: "/js/",
	},
	module: {
		rules: [
			{
				test: /\.css$/,
				use: ['vue-style-loader', 'css-loader'],
			},
			//{
			//	test: /\.scss$/,
			//	use: ['vue-style-loader', 'css-loader', 'sass-loader'],
			//},
			//{
			//	test: /\.vue$/,
			//	loader: 'vue-loader',
			//},
			{
				test: /\.js$/,
				loader: 'babel-loader',
				exclude: /node_modules/,
			},
			{
				test: /\.(png|jpg|gif|svg|woff|woff2|eot|ttf)$/,
				loader: 'url-loader',
			},
		],
	},
	plugins: [
		//new VueLoaderPlugin(),
		new CleanWebpackPlugin(),
		new webpack.ProvidePlugin({
			$: "jquery",
			jQuery: "jquery"
		  })
	],
	resolve: {
		//extensions: ['*', '.js', '.vue'],
		extensions: ['*', '.js'],
	},
	//node: {
	//	fs: 'empty'
	//},
	//target: 'node'
}
