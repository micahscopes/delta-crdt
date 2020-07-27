// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as $$Map from "bs-platform/lib/es6/map.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";

function ChoosableOfBaseOrderedType(T) {
  var empty = T.empty;
  var choose = function (t, t$prime) {
    var comparison = Curry._2(T.compare, t, t$prime);
    if (comparison < 0) {
      return /* Left */Block.__(0, [t]);
    } else if (comparison > 0) {
      return /* Right */Block.__(1, [t$prime]);
    } else if (comparison === 0) {
      return /* Either */Block.__(2, [
                t,
                t$prime
              ]);
    } else {
      return /* Neither */Block.__(3, [
                t,
                t$prime
              ]);
    }
  };
  var join = function (t, t$prime) {
    var match = choose(t, t$prime);
    if (match.tag === /* Neither */3) {
      return empty;
    } else {
      return match[0];
    }
  };
  return {
          empty: empty,
          compare: T.compare,
          choose: choose,
          join: join
        };
}

var compare = Caml_obj.caml_compare;

var Int = {
  empty: 0,
  compare: compare
};

function choose(t, t$prime) {
  var comparison = Caml_obj.caml_compare(t, t$prime);
  if (comparison < 0) {
    return /* Left */Block.__(0, [t]);
  } else if (comparison > 0) {
    return /* Right */Block.__(1, [t$prime]);
  } else if (comparison === 0) {
    return /* Either */Block.__(2, [
              t,
              t$prime
            ]);
  } else {
    return /* Neither */Block.__(3, [
              t,
              t$prime
            ]);
  }
}

function join(t, t$prime) {
  var match = choose(t, t$prime);
  if (match.tag === /* Neither */3) {
    return 0;
  } else {
    return match[0];
  }
}

var IntState = {
  empty: 0,
  compare: compare,
  choose: choose,
  join: join
};

function Make(Id, State) {
  var replica = function (id) {
    return {
            id: id,
            state: State.empty
          };
  };
  var join = function (p, q) {
    var match = p.id;
    var match$1 = q.id;
    var id;
    if (match !== undefined) {
      var id$1 = Caml_option.valFromOption(match);
      id = Caml_option.some(id$1);
    } else {
      id = match$1 !== undefined ? Caml_option.some(Caml_option.valFromOption(match$1)) : undefined;
    }
    var state = Curry._2(State.join, p.state, q.state);
    return {
            id: id,
            state: state
          };
  };
  var patchOfState = function (idOpt, state) {
    var id = idOpt !== undefined ? Caml_option.valFromOption(idOpt) : undefined;
    return {
            id: id,
            state: state
          };
  };
  var mutate = function (replica, deltaMutation) {
    var delta = patchOfState(undefined, deltaMutation);
    var match = replica.id;
    if (match !== undefined) {
      return /* Result */Block.__(0, [
                /* replica */join(replica, delta),
                /* delta */delta
              ]);
    } else {
      return /* Invalid */Block.__(1, [
                /* replica */replica,
                /* delta */delta
              ]);
    }
  };
  return {
          replica: replica,
          join: join,
          patchOfState: patchOfState,
          mutate: mutate
        };
}

function $$Map$1(Key, Value) {
  var Data = $$Map.Make({
        compare: Key.compare
      });
  var join = function (m, m$prime) {
    return Curry._3(Data.merge, (function (param, mState, mState$prime) {
                  if (mState === undefined) {
                    if (mState$prime !== undefined) {
                      return Caml_option.some(Curry._2(Value.join, Caml_option.valFromOption(mState$prime), Value.empty));
                    } else {
                      return ;
                    }
                  }
                  var x = Caml_option.valFromOption(mState);
                  if (mState$prime !== undefined) {
                    return Caml_option.some(Curry._2(Value.join, x, Caml_option.valFromOption(mState$prime)));
                  } else {
                    return Caml_option.some(Curry._2(Value.join, x, Value.empty));
                  }
                }), m, m$prime);
  };
  return {
          Data: Data,
          empty: Data.empty,
          join: join
        };
}

function SimpleMap(Key, Value) {
  var empty = Key.empty;
  var choose = function (t, t$prime) {
    var comparison = Curry._2(Key.compare, t, t$prime);
    if (comparison < 0) {
      return /* Left */Block.__(0, [t]);
    } else if (comparison > 0) {
      return /* Right */Block.__(1, [t$prime]);
    } else if (comparison === 0) {
      return /* Either */Block.__(2, [
                t,
                t$prime
              ]);
    } else {
      return /* Neither */Block.__(3, [
                t,
                t$prime
              ]);
    }
  };
  var join = function (t, t$prime) {
    var match = choose(t, t$prime);
    if (match.tag === /* Neither */3) {
      return empty;
    } else {
      return match[0];
    }
  };
  var partial_arg_compare = Key.compare;
  var Data = $$Map.Make({
        compare: partial_arg_compare
      });
  var join$1 = function (m, m$prime) {
    return Curry._3(Data.merge, (function (param, mState, mState$prime) {
                  if (mState === undefined) {
                    if (mState$prime !== undefined) {
                      return Caml_option.some(Curry._2(Value.join, Caml_option.valFromOption(mState$prime), Value.empty));
                    } else {
                      return ;
                    }
                  }
                  var x = Caml_option.valFromOption(mState);
                  if (mState$prime !== undefined) {
                    return Caml_option.some(Curry._2(Value.join, x, Caml_option.valFromOption(mState$prime)));
                  } else {
                    return Caml_option.some(Curry._2(Value.join, x, Value.empty));
                  }
                }), m, m$prime);
  };
  return {
          Data: Data,
          empty: Data.empty,
          join: join$1
        };
}

function Pair(A, B) {
  var empty_000 = A.empty;
  var empty_001 = B.empty;
  var empty = /* tuple */[
    empty_000,
    empty_001
  ];
  var join = function (param, param$1) {
    return /* tuple */[
            Curry._2(A.join, param[0], param$1[0]),
            Curry._2(B.join, param[1], param$1[1])
          ];
  };
  return {
          empty: empty,
          join: join
        };
}

function LexicographicPair(A, B) {
  var empty_000 = A.empty;
  var empty_001 = B.empty;
  var empty = /* tuple */[
    empty_000,
    empty_001
  ];
  var join = function (param, param$1) {
    var b$prime = param$1[1];
    var b = param[1];
    var a = Curry._2(A.choose, param[0], param$1[0]);
    switch (a.tag | 0) {
      case /* Left */0 :
          return /* tuple */[
                  a[0],
                  b
                ];
      case /* Right */1 :
          return /* tuple */[
                  a[0],
                  b$prime
                ];
      case /* Either */2 :
          return /* tuple */[
                  a[0],
                  Curry._2(B.join, b, b$prime)
                ];
      case /* Neither */3 :
          return /* tuple */[
                  Curry._2(A.join, a[0], a[1]),
                  B.empty
                ];
      
    }
  };
  return {
          empty: empty,
          join: join
        };
}

export {
  ChoosableOfBaseOrderedType ,
  Int ,
  IntState ,
  Make ,
  $$Map$1 as $$Map,
  SimpleMap ,
  Pair ,
  LexicographicPair ,
  
}
/* No side effect */
