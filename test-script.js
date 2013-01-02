/**
 * Test comment
 */
var test = function() {

  /**
   * Outter var
   */
  var outter;

  var newFn = (function () {
    var inner1, inner2;


  });

};

/*
 * More comments
 */
function trial() {

}


// Closures

var sum = (function(var1, var2){
  var aSumVar;
  return var1 + var2;
});



var product = function(var1, var2) {
  var aProVar;
  return var1 * var2;
};
