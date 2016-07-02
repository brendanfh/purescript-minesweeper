"use strict";

//module Main

exports.requestAnimationFrame = function(f) {
    return function() {
        window.requestAnimationFrame(function(dt) {
            f();
        });
    }
};

exports.onMouseUp = function(canvas) {
    return function(button) {
        return function(f) {
            return function() {
                canvas.addEventListener("mouseup", function(e) {
                    e.preventDefault();
                    if (e.button == button) {
                        f(e.layerX)(e.layerY)();
                    }
                    return false;
                }, false);
                if (button == 2) {
                    canvas.oncontextmenu = function() { return false; }
                }
            }
        }
    }
}