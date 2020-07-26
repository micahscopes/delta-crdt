// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as Crdt$DeltaCrdts from "./Crdt.bs.js";

function State(Id) {
  var partial_arg_empty = Crdt$DeltaCrdts.IntState.empty;
  var partial_arg_choose = Crdt$DeltaCrdts.IntState.choose;
  var partial_arg_join = Crdt$DeltaCrdts.IntState.join;
  var partial_arg = {
    empty: partial_arg_empty,
    choose: partial_arg_choose,
    join: partial_arg_join
  };
  var partial_arg$1 = Crdt$DeltaCrdts.LexicographicPair;
  var include = (function (param) {
        return partial_arg$1(partial_arg, param);
      })({
        empty: Crdt$DeltaCrdts.IntState.empty,
        join: Crdt$DeltaCrdts.IntState.join
      });
  var increment = function (param) {
    return /* tuple */[
            param[0],
            param[1] + 1 | 0
          ];
  };
  var decrement = function (param) {
    return /* tuple */[
            param[0] + 1 | 0,
            param[1] - 1 | 0
          ];
  };
  var LexState_empty = include.empty;
  var LexState_join = include.join;
  var LexState = {
    empty: LexState_empty,
    join: LexState_join,
    increment: increment,
    decrement: decrement
  };
  var partial_arg$2 = Crdt$DeltaCrdts.SimpleMap;
  var $$Map = partial_arg$2(Id, LexState);
  var increment$1 = function (state, id) {
    return Curry._2($$Map.Data.singleton, id, increment(Curry._2($$Map.Data.find, id, state)));
  };
  var decrement$1 = function (state, id) {
    return Curry._2($$Map.Data.singleton, id, decrement(Curry._2($$Map.Data.find, id, state)));
  };
  var value = function (state) {
    return Curry._3($$Map.Data.fold, (function (param, param$1, accum) {
                  return param$1[1] + accum | 0;
                }), state, 0);
  };
  return {
          LexState: LexState,
          $$Map: $$Map,
          Data: $$Map.Data,
          empty: $$Map.empty,
          join: $$Map.join,
          increment: increment$1,
          decrement: decrement$1,
          value: value
        };
}

function Make(Id) {
  var partial_arg_empty = Crdt$DeltaCrdts.IntState.empty;
  var partial_arg_choose = Crdt$DeltaCrdts.IntState.choose;
  var partial_arg_join = Crdt$DeltaCrdts.IntState.join;
  var partial_arg = {
    empty: partial_arg_empty,
    choose: partial_arg_choose,
    join: partial_arg_join
  };
  var partial_arg$1 = Crdt$DeltaCrdts.LexicographicPair;
  var include = (function (param) {
        return partial_arg$1(partial_arg, param);
      })({
        empty: Crdt$DeltaCrdts.IntState.empty,
        join: Crdt$DeltaCrdts.IntState.join
      });
  var increment = function (param) {
    return /* tuple */[
            param[0],
            param[1] + 1 | 0
          ];
  };
  var decrement = function (param) {
    return /* tuple */[
            param[0] + 1 | 0,
            param[1] - 1 | 0
          ];
  };
  var LexState_empty = include.empty;
  var LexState_join = include.join;
  var LexState = {
    empty: LexState_empty,
    join: LexState_join,
    increment: increment,
    decrement: decrement
  };
  var partial_arg$2 = Crdt$DeltaCrdts.SimpleMap;
  var $$Map = partial_arg$2(Id, LexState);
  var empty = $$Map.empty;
  var join = $$Map.join;
  var increment$1 = function (state, id) {
    return Curry._2($$Map.Data.singleton, id, increment(Curry._2($$Map.Data.find, id, state)));
  };
  var decrement$1 = function (state, id) {
    return Curry._2($$Map.Data.singleton, id, decrement(Curry._2($$Map.Data.find, id, state)));
  };
  var value = function (state) {
    return Curry._3($$Map.Data.fold, (function (param, param$1, accum) {
                  return param$1[1] + accum | 0;
                }), state, 0);
  };
  var State_Data = $$Map.Data;
  var State = {
    LexState: LexState,
    $$Map: $$Map,
    Data: State_Data,
    empty: empty,
    join: join,
    increment: increment$1,
    decrement: decrement$1,
    value: value
  };
  var partial_arg$3 = {
    compare: Id.compare
  };
  var partial_arg$4 = Crdt$DeltaCrdts.Make;
  var include$1 = (function (param) {
        return partial_arg$4(partial_arg$3, param);
      })({
        empty: empty,
        join: join
      });
  var mutate = include$1.mutate;
  var increment$2 = function (replica) {
    var id = replica.id;
    if (id !== undefined) {
      return Curry._2(mutate, replica, increment$1(replica.state, Caml_option.valFromOption(id)));
    } else {
      return /* Invalid */Block.__(1, [
                /* replica */replica,
                /* delta */undefined
              ]);
    }
  };
  var decrement$2 = function (replica) {
    var id = replica.id;
    if (id !== undefined) {
      return Curry._2(mutate, replica, decrement$1(replica.state, Caml_option.valFromOption(id)));
    } else {
      return /* Invalid */Block.__(1, [
                /* replica */replica,
                /* delta */undefined
              ]);
    }
  };
  var value$1 = function (param) {
    return value(param.state);
  };
  return {
          State: State,
          replica: include$1.replica,
          join: include$1.join,
          patchOfState: include$1.patchOfState,
          mutate: mutate,
          increment: increment$2,
          decrement: decrement$2,
          value: value$1
        };
}

export {
  State ,
  Make ,
  
}
/* No side effect */
