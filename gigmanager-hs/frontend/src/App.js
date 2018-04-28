/* global gapi */

import React, { Component } from 'react';
import './App.css';
import * as api from './ApiFunctions.js';

window.api = api;

var GoogleAuth;
var SCOPE = 'email https://www.googleapis.com/auth/calendar'
function initClient() {
  // Initialize the gapi.client object, which app uses to make API requests.
  // Get API key and client ID from API Console.
  // 'scope' field specifies space-delimited list of access scopes.
  window.gapi.client.init({
      'apiKey': 'AIzaSyCim6EDPS8-h-3cuhhpke8UIWAnQkIr4iM',
      'discoveryDocs': [],
      'clientId': '548142593260-q8s4d9d4hsqmb30sg3ljcdd4m2km2nav.apps.googleusercontent.com',
      'scope': SCOPE
  }).then(function () {
    console.log("Client inited!")
    GoogleAuth = window.gapi.auth2.getAuthInstance();

    // Listen for sign-in state changes.
    GoogleAuth.isSignedIn.listen(setSigninStatus);

    // Handle initial sign-in state. (Determine if user is already signed in.)
    var user = GoogleAuth.currentUser.get();
    setSigninStatus();

    /*
    // Call handleAuthClick function when user clicks on
    //      "Sign In/Authorize" button.
    $('#sign-in-or-out-button').click(function() {
      handleAuthClick();
    });
    $('#revoke-access-button').click(function() {
      revokeAccess();
    });
    */
  }).catch((e) => console.log("Init failed: ", e));
}

function setSigninStatus(isSignedIn) {
  var user = GoogleAuth.currentUser.get();
  // This doesn't cache. Only call if need to refresh token
  /*
  GoogleAuth.grantOfflineAccess({scope: "https://www.googleapis.com/auth/calendar"}).then((resp) => {
    console.log(resp, resp.code);
  }).catch((e) => console.log("offline access error", e));
  */
  window.user = user;
  console.log("User", user.getBasicProfile().getEmail());
  var token = user.getAuthResponse().id_token;
  console.log("SENDING", token);
  fetch('http://localhost:8000/quotes', {
    headers: new Headers({
      'Authorization': 'Bearer ' + token
    })
  }).then((resp) => {
    resp.json().then((data) => console.log(data))
  });
  var isAuthorized = user.hasGrantedScopes(SCOPE);
  console.log(isAuthorized);
  if (isAuthorized) {
    /*
    $('#sign-in-or-out-button').html('Sign out');
    $('#revoke-access-button').css('display', 'inline-block');
    $('#auth-status').html('You are currently signed in and have granted ' +
        'access to this app.');
        */
  } else {
    /*
    $('#sign-in-or-out-button').html('Sign In/Authorize');
    $('#revoke-access-button').css('display', 'none');
    $('#auth-status').html('You have not authorized this app or you are ' +
        'signed out.');
        */
  }
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      quotes: []
    }
  }

  componentDidMount() {
    const script = document.createElement("script");
    script.src = "https://apis.google.com/js/api.js";

    script.onload = () => {
      console.log("loaded!")
      console.log(window.gapi)
      gapi.load('client', () => initClient())
    }
    document.body.appendChild(script)

    /*
    api.getQuotes()
      .then((resp) => resp.json())
      .then((data) => { console.log(data); this.setState({"quotes": data})})
      */
  }

  handleAuthClick() {
    if (GoogleAuth.isSignedIn.get()) {
      // User is authorized and has clicked 'Sign out' button.
      GoogleAuth.signOut();
    } else {
      // User is not signed in. Start Google auth flow.
      GoogleAuth.signIn();
    }
  }


  render() {
    return (
      <div className="App">
        <a href='#' onClick={this.handleAuthClick}>Login</a>
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
