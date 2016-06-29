"use strict";

//module Main

exports.requestAnimationFrame = function(f) {
    return function() {
        window.requestAnimationFrame(function(dt) {
            f();
        });
    }
};
