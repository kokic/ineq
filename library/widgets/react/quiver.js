
import * as React from 'react';

const encoder = new TextEncoder();

const quiver_base64 = s => btoa(String.fromCharCode(...encoder.encode(s)))

export default function (props) {
  
  // return React.createElement('p', {}, JSON.stringify(props))
  const base64 = quiver_base64(props.data)
  return React.createElement('iframe', {
    src: `https://q.uiver.app/#q=${base64}&embed`,
  })
}

