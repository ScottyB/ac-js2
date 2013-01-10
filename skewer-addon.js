/**
 * @fileOverview Completion request handler for skewer.js
 * @requires skewer
 * @version 1.0
 */

/**
 * Handles a completion request from Emacs.
 * @param request The request object sent by Emacs
 * @returns The completions and init values to be returned to Emacs
 */
skewer.fn.complete = function(request) {
    var value = {},
        result =  {
            type : request.type,
            id : request.id,
            strict : request.strict,
            status : "success"
        },

        addValues = function(obj) {
            // loop over non enumerable properties
            var keys = Object.getOwnPropertyNames(obj);
            for (var i = 0; i < keys.length; i++) {
                var key = keys[i];
                if (Object.prototype.toString.call(obj[key]) === "[object Function]") {
                    var str = obj[key].toString();
                    if (str.indexOf('[native code]') !== -1) {
                        value [key] = str;
                    } else {
                        var pos = str.indexOf(")");
                        value [key] = str.substring(0, pos +1);
                    }
                } else if (typeof obj[key] === "number") {
                    if (!(obj instanceof Array)) {
                        value[key] = obj[key].toString();
                    }
                } else if (typeof obj[key] === "string") {
                    value[key] = obj[key].toString();
                } else if(obj[key] === true) {
                    value[key] = "true";
                } else if (obj[key] === false) {
                    value[key] = "false";
                } else {
                    value[key] = "";
                }
            }
        };
    try {
        var obj = (eval, eval)(request.eval);
        if (typeof obj === "object") {
            addValues(obj);
            while (request.prototypes && (obj = Object.getPrototypeOf(obj)) !== null) {
                addValues(obj);
            }
        } else if (typeof obj === "function"){
            addValues(obj);
            addValues(Object.getPrototypeOf(obj));
            if (request.prototypes) {
                var protoObject = Object.getPrototypeOf(obj.prototype);
                if (protoObject !== null) {
                    addValues(protoObject);
                } else {
                    addValues(obj.prototype);
                }
            }
        }
        result.value = value;
    } catch (error){
        skewer.errorResult(error, result, request);
    }
    return result;
};
