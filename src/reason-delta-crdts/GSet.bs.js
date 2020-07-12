// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as $$Set from "bs-platform/lib/es6/set.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as Crdt$DeltaCrdts from "./Crdt.bs.js";

function Make(ElementType) {
  var State = $$Set.Make(ElementType);
  var join = function (p, q) {
    return Curry._2(State.union, p, q);
  };
  var empty = State.empty;
  var Patch = {
    join: join,
    empty: empty
  };
  var Core = Crdt$DeltaCrdts.Make({
        empty: empty,
        join: join
      });
  var replica = function (id) {
    return {
            id: Caml_option.some(id),
            state: empty
          };
  };
  var elements = function (patch) {
    return patch.state;
  };
  var insert = function (replica, element) {
    var id = replica.id;
    if (id === undefined) {
      return /* Invalid */Block.__(1, [replica]);
    }
    var delta = Curry._2(State.add, element, State.empty);
    var state = Curry._2(State.add, element, replica.state);
    return /* Result */Block.__(0, [
              /* replica */{
                id: Caml_option.some(Caml_option.valFromOption(id)),
                state: state
              },
              /* delta */{
                id: undefined,
                state: delta
              }
            ]);
  };
  return {
          State: State,
          Patch: Patch,
          Core: Core,
          join: Core.join,
          replica: replica,
          elements: elements,
          insert: insert
        };
}

export {
  Make ,
  
}
/* No side effect */