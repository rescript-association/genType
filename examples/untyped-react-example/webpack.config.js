const path = require("path");

const isProd = process.env.NODE_ENV === "production";

module.exports = {
  entry: {
    main: "./src/main.js"
  },
  mode: isProd ? "production" : "development",
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: "[name].js"
  },
  module: {
    rules: [
      {
        test: /(\.bs\.)?\.js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: "babel-loader",
          options: {
            presets: [
              "@babel/preset-env",
              "@babel/preset-react"
            ]
          }
        }
      }
    ]
  }
};
