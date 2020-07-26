// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Tape from "bs-tape/src/Tape.bs.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as $$String from "bs-platform/lib/es6/string.js";
import * as TwoPSet$DeltaCrdts from "../../src/reason-delta-crdts/TwoPSet.bs.js";
import * as Caml_builtin_exceptions from "bs-platform/lib/es6/caml_builtin_exceptions.js";

Tape.test("test that we can add and remove an element only once", (function (t) {
        var partial_arg = {
          compare: $$String.compare
        };
        var partial_arg$1 = TwoPSet$DeltaCrdts.Make;
        var T = (function (param) {
              return partial_arg$1(partial_arg, param);
            })({
              compare: $$String.compare
            });
        var marge = Curry._1(T.replica, "marge");
        var el = "lol";
        var elSet = Curry._1(T.State.Data.singleton, el);
        var match = Curry._2(T.insert, marge, el);
        if (match.tag) {
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "TestTwoPSet.re",
                  11,
                  6
                ]
              ];
        }
        var marge$1 = match[/* replica */0];
        var arg = t.ok;
        Curry._2(arg, undefined, Curry._2(T.State.Data.equal, elSet, Curry._1(T.elements, marge$1)));
        var match$1 = Curry._2(T.insert, marge$1, el);
        if (match$1.tag) {
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "TestTwoPSet.re",
                  14,
                  6
                ]
              ];
        }
        var marge$2 = match$1[/* replica */0];
        var arg$1 = t.ok;
        Curry._2(arg$1, undefined, Curry._2(T.State.Data.equal, elSet, Curry._1(T.elements, marge$2)));
        var match$2 = Curry._2(T.remove, marge$2, el);
        if (match$2.tag) {
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "TestTwoPSet.re",
                  17,
                  6
                ]
              ];
        }
        var marge$3 = match$2[/* replica */0];
        var arg$2 = t.ok;
        Curry._2(arg$2, undefined, Curry._2(T.State.Data.equal, T.State.Data.empty, Curry._1(T.elements, marge$3)));
        var match$3 = Curry._2(T.remove, marge$3, el);
        if (match$3.tag) {
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "TestTwoPSet.re",
                  20,
                  6
                ]
              ];
        }
        var marge$4 = match$3[/* replica */0];
        var arg$3 = t.ok;
        Curry._2(arg$3, undefined, Curry._2(T.State.Data.equal, T.State.Data.empty, Curry._1(T.elements, marge$4)));
        var match$4 = Curry._2(T.insert, marge$4, el);
        if (match$4.tag) {
          throw [
                Caml_builtin_exceptions.match_failure,
                /* tuple */[
                  "TestTwoPSet.re",
                  23,
                  6
                ]
              ];
        }
        var arg$4 = t.ok;
        Curry._2(arg$4, undefined, Curry._2(T.State.Data.equal, T.State.Data.empty, Curry._1(T.elements, match$4[/* replica */0])));
        return Curry._1(t.endTest, undefined);
      }));

export {
  
}
/*  Not a pure module */
