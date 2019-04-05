/*
 import * as PropTypes from 'prop-types';

 import * as ReasonReact from 'reason-react/src/ReasonReact.js';

 export const App = ReasonReact.wrapReasonForJs(
   AppBS.component,
   (function _(jsProps) {
      return Curry._2(AppBS.make, jsProps.title, jsProps.children);
   }));

 App.propTypes = {
   title: PropTypes.string.isRequired,
 };

 */

let component = ReasonReact.statelessComponent("App");

type person = {
  name: string,
  age: int,
};

[@genType]
let make = (~callback=() => (), ~person, ~title, _children) => {
  ...component,
  render: _self => {
    callback();
    <div>
      {ReasonReact.string(
         "Test Component Title:" ++ title ++ " Name:" ++ person.name,
       )}
    </div>;
  },
};