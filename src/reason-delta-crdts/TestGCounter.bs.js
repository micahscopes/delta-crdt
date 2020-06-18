// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Tape from "bs-tape/src/Tape.bs.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as $$String from "bs-platform/lib/es6/string.js";
import * as GCounter$DeltaCrdts from "./GCounter.bs.js";

Tape.test("test that we can increment the counter", (function (t) {
        var CounterType = GCounter$DeltaCrdts.Make({
              compare: $$String.compare
            });
        var match = Curry._1(CounterType.increment, Curry._1(CounterType.replica, "marge"));
        Curry._2(t.ok, undefined, Curry._1(CounterType.value, match[0]) === 1);
        return Curry._1(t.endTest, undefined);
      }));

Tape.test("test that we can join deltas/replicas", (function (t) {
        var CounterType = GCounter$DeltaCrdts.Make({
              compare: $$String.compare
            });
        var alice = Curry._1(CounterType.replica, "alice");
        var bob = Curry._1(CounterType.replica, "bob");
        var marge = Curry._1(CounterType.replica, "marge");
        var match = Curry._1(CounterType.increment, alice);
        var dAlice = match[1];
        var alice$1 = match[0];
        var match$1 = Curry._1(CounterType.increment, bob);
        var dBob = match$1[1];
        var bob$1 = match$1[0];
        Curry._2(t.ok, undefined, Curry._1(CounterType.value, Curry._2(CounterType.join, alice$1, dBob)) === Curry._1(CounterType.value, Curry._2(CounterType.join, bob$1, dAlice)));
        Curry._2(t.ok, undefined, Curry._1(CounterType.value, Curry._2(CounterType.join, alice$1, dBob)) === Curry._1(CounterType.value, Curry._2(CounterType.join, alice$1, bob$1)));
        var delta = Curry._2(CounterType.join, dAlice, dBob);
        var marge$1 = Curry._2(CounterType.join, marge, delta);
        Curry._2(t.ok, undefined, Curry._1(CounterType.value, marge$1) === Curry._1(CounterType.value, Curry._2(CounterType.join, alice$1, bob$1)));
        return Curry._1(t.endTest, undefined);
      }));

export {
  
}
/*  Not a pure module */
