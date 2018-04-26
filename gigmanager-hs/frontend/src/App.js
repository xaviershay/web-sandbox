import React, { Component } from 'react';
import logo from './logo.svg';
import './App.css';
import * as api from './ApiFunctions.js';

class App extends Component {
  render() {
    window.f = api.getQuotesById(123)
    window.f.then((data) => console.log(data.json()))
    //f.then((data) => console.log(data)).error(() => console.log("error"))

    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to React</h1>
        </header>
        <p className="App-intro">
          To get started, edit <code>src/App.js</code> and save to reload.
        </p>
      </div>
    );
  }
}

export default App;
