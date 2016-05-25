var request = window.superagent;

function putperson (body) {
  console.log('Data: ', body);
  $('#app').text(body.name);
}

function getpersoncb (err, res) {
  if (err) {
    console.log('error: ', err);
  } else {
    putperson(res.body);
  }
}

function getperson () {
  request
    .get('/jsontext')
    .set('Accept', 'application/json')
    .end(getpersoncb);
}

getperson();
