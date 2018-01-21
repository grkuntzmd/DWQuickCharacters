const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require("path");
const UglifyJsPlugin = require("uglifyjs-webpack-plugin");
const webpack = require("webpack");

module.exports = {
    entry: {
        app: [
            "./src/index.js"
        ]
    },

    output: {
        path: path.resolve(__dirname + "/docs"),
        filename: "[name].js"
    },

    module: {
        rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "elm-webpack-loader?verbose=true&warn=true"
            }, {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: "file-loader?name=[name].[ext]"
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: "url-loader?limit=10000&mimetype=application/font-woff"
            },
            {
                test: /\.(ttf|eot|svg|ico)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: "file-loader"
            },
            {
                test: /\.css$/,
                use: [
                    "style-loader",
                    "css-loader"
                ]
            }, {
                test: /\.scss$/,
                loaders: ["style-loader", "css-loader", "sass-loader"]
            }
        ],

        noParse: /\.elm$/
    },

    devServer: {
        inline: true,
        stats: { colors: true }
    },

    plugins: [
        new CopyWebpackPlugin([
            { from: "src/favicon.png" },
            { from: "src/logo.svg" }
        ]),
        new UglifyJsPlugin({
            cache: true,
            parallel: true
        })
    ]
};