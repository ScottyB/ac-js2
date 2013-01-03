/**
 * A comment for the Setup object
 */
var math = function() {

  var constants = {
    PI : 3.14,
    E : 2.72
  };

  return {
    /**
     * Add two numbers together
     */
    sum : function(num1, num2) {return num1 + num2;},

    /**
     * Subtract two numbers
     */
    minus : function(num1, num2) {return num1 - num2;}
  };

};

var moreMaths = {};

moreMaths.product = function (num1, num2){
  return num1 * num2;
};

moreMaths.divide = function (numerator, denominator){
  return numerator / denominator;
};

var someMath = math();
