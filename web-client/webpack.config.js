var path = require("path");
var webpack = require("webpack");
var merge = require("webpack-merge");
var autoprefixer = require("autoprefixer");
var ExtractTextPlugin = require("extract-text-webpack-plugin");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var CopyWebpackPlugin = require("copy-webpack-plugin");

const LOCAL_GRAPHQL_URL = "http://localhost:8888/graphql";
const AWS_GRAPHQL_URL = "https://stayhabbyserver.xyz/graphql";

const prod = "production";
const dev = "development";

// determine build env
const TARGET_ENV = process.env.npm_lifecycle_event === "build" ? prod : dev;
const isDev = TARGET_ENV == dev;
const isProd = TARGET_ENV == prod;

// entry and output path/filename variables
const entryPath = path.join(__dirname, "src/index.js");
const outputPath = path.join(__dirname, "dist");
const outputFilename = isProd ? "[name]-[hash].js" : "[name].js";

console.log("WEBPACK GO! Building for " + TARGET_ENV);

// common webpack config (valid for dev and prod)
var commonConfig = {
  output: {
    path: outputPath,
    filename: `${outputFilename}`,
  },
  resolve: {
    extensions: [".js", ".elm"],
    modules: ["node_modules"],
  },
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.(eot|ttf|woff|woff2|svg)$/,
        use: "file-loader?publicPath=../../&name=[hash].[ext]",
      },
      {
        test: /\index.js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["es2015"],
          },
        },
      },
    ],
  },
  plugins: [
    new webpack.LoaderOptionsPlugin({
      options: {
        postcss: [autoprefixer()],
      },
    }),
    new webpack.DefinePlugin({
      __WEBPACK_CONSTANT_API_BASE_URL__: JSON.stringify(LOCAL_GRAPHQL_URL),
    }),
    new HtmlWebpackPlugin({
      template: "src/index.html",
      inject: "body",
      filename: "index.html",
    }),
  ],
};

// additional webpack settings for local env (when invoked by 'npm start')
if (isDev === true) {
  module.exports = merge(commonConfig, {
    entry: ["webpack-dev-server/client?http://localhost:8080", entryPath],
    devServer: {
      // serve index.html in place of 404 responses
      historyApiFallback: true,
      contentBase: "./dist",
      hot: true,
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: "elm-webpack-loader",
              options: {
                verbose: true,
                debug: false,
              },
            },
          ],
        },
        {
          test: /\.sc?ss$/,
          use: ["style-loader", "css-loader", "postcss-loader", "sass-loader"],
        },
      ],
    },
    plugins: [
      new CopyWebpackPlugin([
        {
          from: "assets/",
          to: "assets/",
        },
      ]),
    ],
  });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (isProd === true) {
  module.exports = merge(commonConfig, {
    entry: entryPath,
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: "elm-webpack-loader",
        },
        {
          test: /\.sc?ss$/,
          use: ExtractTextPlugin.extract({
            fallback: "style-loader",
            use: ["css-loader", "postcss-loader", "sass-loader"],
          }),
        },
      ],
    },
    plugins: [
      new ExtractTextPlugin({
        filename: "[name]-[hash].css",
        allChunks: true,
      }),
      new CopyWebpackPlugin([
        {
          from: "assets/",
          to: "assets/",
        },
      ]),

      // extract CSS into a separate file
      // minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
        minimize: true,
        compressor: {
          warnings: false,
        },
        // mangle:  true
      }),
    ],
  });
}
