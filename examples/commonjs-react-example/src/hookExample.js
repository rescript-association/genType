/* @flow strict */

const React = require("react");

const make: (props: {|
  +show: boolean,
  +Message?: string,
|}) => React.Node = (props) => {
  return props.show ? (
    <div>
      {props.Message == undefined ? "no message passed" : props.Message}
    </div>
  ) : null;
};

module.exports = make;
