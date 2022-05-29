var storedState = localStorage.getItem("life-in-weeks-model");
var startingState = storedState ? JSON.parse(storedState) : null;

var app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: startingState,
});

app.ports.storeModel.subscribe(function (model) {
  var modelJson = JSON.stringify(model);
  localStorage.setItem("life-in-weeks-model", modelJson);
});
