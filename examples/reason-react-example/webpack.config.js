const path = require("path");

const isProd = process.env.NODE_ENV === "production";


const CircularDependencyPlugin = require('circular-dependency-plugin')

module.exports = {
  entry: {
    interop: "./src/interop/InteropRoot.js"
  },
  plugins: [
    new CircularDependencyPlugin({
      // exclude detection of files based on a RegExp
      exclude: /a\.js|node_modules/,
      // add errors to webpack instead of warnings
      failOnError: true,
      // allow import cycles that include an asyncronous import,
      // e.g. via import(/* webpackMode: "weak" */ './file.js')
      allowAsyncCycles: false,
      // set the current working directory for displaying module paths
      cwd: process.cwd(),
    })
  ],
  mode: isProd ? "production" : "development",
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: "[name].js"
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: "babel-loader",
          options: {
            presets: [
              "@babel/preset-env",
              "@babel/preset-flow",
              "@babel/preset-react"
            ]
          }
        }
      },
      {
        loader: "extension-replace-loader",
        query: {
          exts: [{ from: ".re", to: ".re.js" }]
        }
      }
    ]
  }
}
