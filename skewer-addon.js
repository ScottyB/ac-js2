/**
 * @fileOverview Completion request handler for skewer.js
 * @requires skewer
 * @version 1.0
 */

var ScopeObject = {};

/**
 * Handles a completion request from Emacs.
 * @param request The request object sent by Emacs
 * @returns The completions and init values to be returned to Emacs
 */
skewer.fn.complete = function(request) {
    var result =  {
        type : request.type,
        id : request.id,
        strict : request.strict,
        status : "success"
    },

        /**
         * Methods for generating candidates
         */
        METHOD = {
            EVAL : 0,
            GLOBAL : 1,
            RESET : 2
        },

        removeProperties = function(object) {
            for(var property in object) {
                if (object.hasOwnProperty(property)) {
                    delete object[property];
                }
            }
        },

        globalCompletion = function() {
            var global = Function('return this')(),
                keys = Object.keys(global);
            buildCandidates(global, keys);
            buildCandidates(ScopeObject);
        },

        evalCompletion = function(evalObject) {
            var obj = (eval, eval)(evalObject);
            if (typeof obj === "object") {
                buildCandidates(obj);
                while (request.prototypes && (obj = Object.getPrototypeOf(obj)) !== null) {
                    buildCandidates(obj);
                }
            } else if (typeof obj === "function"){
                buildCandidates(obj);
                buildCandidates(Object.getPrototypeOf(obj));
                if (request.prototypes) {
                    var protoObject = Object.getPrototypeOf(obj.prototype);
                    if (protoObject !== null) {
                        buildCandidates(protoObject);
                    } else {
                        buildCandidates(obj.prototype);
                    }
                }
            }
        },

        /**
         * Completion candidates sent back to Emacs. Keys are
         * completion candidates the values are the inital items or
         * function interfaces.
         */
        candidates = {},

        /**
         * Build the candiates to return to Emacs.
         * @param obj The object to get candidates from
         * @param items The selected keys from obj to create candidates for
         */
        buildCandidates = function(obj, items) {
            var keys = items || Object.getOwnPropertyNames(obj);
            for (var i = 0; i < keys.length; i++) {
                var key = keys[i];
                if (Object.prototype.toString.call(obj[key]) === "[object Function]") {
                    var str = obj[key].toString();
                    if (str.indexOf('[native code]') !== -1) {
                        candidates[key] = str;
                    } else {
                        var pos = str.indexOf(")");
                        candidates[key] = str.substring(0, pos +1);
                    }
                } else if (typeof obj[key] === "object"){
                    candidates[key] = "[object Object]";
                } else if (typeof obj[key] === "number") {
                    if (!(obj instanceof Array)) {
                        candidates[key] = obj[key].toString();
                    }
                } else if (typeof obj[key] === "string") {
                    candidates[key] = obj[key].toString();
                } else if(obj[key] === true) {
                    candidates[key] = "true";
                } else if (obj[key] === false) {
                    candidates[key] = "false";
                } else {
                    candidates[key] = "";
                }
            }
        };
    try {
        switch (request.method) {
        case METHOD.GLOBAL:
            globalCompletion();
            break;
        case METHOD.RESET:
            removeProperties(ScopeObject);
            break;
        default:
            var keys = Object.getOwnPropertyNames(ScopeObject), found;
            for (var i = 0; i < keys.length; i++) {
                if (keys[i] === request.eval) {
                    evalCompletion(ScopeObject[request.eval]);
                    found = true;
                    break;
                }
            }
            if (!found) {
                evalCompletion(request.eval);
            }
        }
        result.value = candidates;
    } catch (error){
        skewer.errorResult(error, result, request);
    }
    return result;
};
