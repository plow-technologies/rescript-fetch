// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Test = require("rescript-test/src/Test.res.js");
var Core__Option = require("@rescript/core/src/Core__Option.res.js");

function isSome(message, value) {
  if (value !== undefined) {
    return Test.pass(message, undefined);
  } else {
    return Test.fail("Expected Some, got None", undefined);
  }
}

function elementContains(message, element, substring) {
  Test.assertion(message, "elementContains", (function (textContent, substring) {
          return textContent.includes(substring);
        }), Core__Option.mapOr(element, "Not Found", (function (element) {
              return element.textContent;
            })), substring);
}

exports.isSome = isSome;
exports.elementContains = elementContains;
/* Test Not a pure module */
