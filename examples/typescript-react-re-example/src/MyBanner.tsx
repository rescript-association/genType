import * as React from "react";
import "./App.css";

// tslint:disable-next-line:interface-name
export interface Props {
  show: boolean;
  message?: { text: string };
}

class App extends React.PureComponent<Props> {
  public render() {
    if (this.props.show) {
      return (
        <div className="App">
          {"Here's the message from the owner: " +
            (this.props.message === undefined ? "3" : this.props.message.text)}
        </div>
      );
    } else {
      return null;
    }
  }
}

export class TopLevelClass {
  static MiddleLevelElements = {
    MyBannerInternal: App
  };
}

export default App;
