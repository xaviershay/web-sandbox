export function getQuotes() {
  return fetch('http://localhost:8000/quotes');
}

export function getQuotesById(id) {
  return fetch('http://localhost:8000/quotes/' + encodeURIComponent(id) + '');
}

