/* @flow strict */

const React = require("react");

const make = (props: {| +show: boolean, +Message?: string |}) => {
  return props.show ? (
    <div>
      {props.Message == undefined ? "no message passed" : props.Message}
    </div>
  ) : null;
};

module.exports = make;
