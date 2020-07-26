// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as $$Set from "bs-platform/lib/es6/set.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as GSet$DeltaCrdts from "./GSet.bs.js";

function DotType(Id) {
  var next = function (i) {
    return i + 1 | 0;
  };
  var compare = function (param, param$1) {
    var c = Curry._2(Id.compare, param[0], param$1[0]);
    if (c !== 0) {
      return c;
    } else {
      return Caml_obj.caml_compare(param[1], param$1[1]);
    }
  };
  return {
          emptyTime: 0,
          next: next,
          compare: compare
        };
}

function Context(DotType) {
  var Data = $$Set.Make({
        compare: DotType.compare
      });
  var include = GSet$DeltaCrdts.State(Data);
  var max = function (state, id) {
    return Curry._1(Data.max_elt, Curry._2(Data.add, /* tuple */[
                      id,
                      DotType.emptyTime
                    ], Curry._2(Data.filter, (function (param) {
                            return Caml_obj.caml_equal(id, param[0]);
                          }), state)))[1];
  };
  var next = function (state, id) {
    return /* tuple */[
            id,
            Curry._1(DotType.next, max(state, id))
          ];
  };
  return {
          Data: Data,
          empty: include.empty,
          join: include.join,
          insert: include.insert,
          max: max,
          next: next
        };
}

export {
  DotType ,
  Context ,
  
}
/* No side effect */
