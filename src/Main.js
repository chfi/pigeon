"use strict";

exports.setWindow = function(n) {
    return function(a) {
        return function() {
            window[n] = a;
        };
    };
};
