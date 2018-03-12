"using strict";
exports.getRand = function (min){
  return function (max) {
    return Math.floor(Math.random() * (max - min + 1) + min);
  }
}
