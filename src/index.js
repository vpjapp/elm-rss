const repl = require('repl')

// Link to Elm code
var Elm = require('./main').Elm
var main = Elm.Main.init({ flags: new Date().toISOString() })

// Eval function for the repl
function start() {
  main.ports.print.subscribe(function putCallback(data) {
    console.log(data)
  })
}
start()
