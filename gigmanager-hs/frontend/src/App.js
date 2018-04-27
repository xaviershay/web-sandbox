import React, { Component } from 'react';
import './App.css';
import * as api from './ApiFunctions.js';

window.api = api;

class App extends Component {
  constructor() {
    super();
    this.state = {
      quotes: []
    }
  }

  componentDidMount() {
    api.getQuotes()
      .then((resp) => resp.json())
      .then((data) => { console.log(data); this.setState({"quotes": data})})
  }

  render() {
    return (
      <div className="App">
        <ul>
          {this.state.quotes.map((q) =>
            <li key={q.quoteId}>{q.quoteDescription}</li>
          )}
        </ul>
      </div>
    );
  }
}

export default App;
