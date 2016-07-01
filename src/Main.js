"use strict";

//module Main

exports.requestAnimationFrame = function(f) {
    return function() {
        window.requestAnimationFrame(function(dt) {
            f();
        });
    }
};

exports.onMouseUp = function(button) {
    return function(f) {
        return function() {
            document.addEventListener("mouseup", function(e) {
                e.preventDefault();
                if (e.button == button) {
                    f(e.layerX)(e.layerY)();
                }
                return false;
            }, false);
            if (button == 2) {
                document.oncontextmenu = function() { return false; }
            }
        }
    }
}