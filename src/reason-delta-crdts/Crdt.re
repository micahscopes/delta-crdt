module type BaseType = {
  type t;
  let empty: t;
};

module type JoinableState = {
  include BaseType;
  let join: (t, t) => t;
};

module type BaseOrderedType = {
  include BaseType;
  let compare: (t, t) => int;
};

module type OrderedState = {
  include BaseOrderedType;
  let join: (t, t) => t;
};

module type BaseChoosableType = {
  include BaseType;

  type decision =
    | Left(t)
    | Right(t)
    | Either(t, t)
    | Neither(t, t);

  let choose: (t, t) => decision;
};

module type ChoosableState = {
  include BaseChoosableType;
  let join: (t, t) => t;
};

module ChoosableOfBaseOrderedType = (T: BaseOrderedType) => {
  include T;

  type decision =
    | Left(T.t)
    | Right(T.t)
    | Either(T.t, T.t)
    | Neither(T.t, T.t);

  let choose = (t, t') => {
    let comparison = T.compare(t, t');
    if (comparison < 0) {
      Left(t);
    } else if (comparison > 0) {
      Right(t');
    } else if (comparison == 0) {
      Either(t, t');
    } else {
      Neither(t, t');
    };
  };

  let join = (t, t') =>
    switch (choose(t, t')) {
    | Left(x)
    | Right(x)
    | Either(x, _) => x
    | _ => empty
    };
};

module Int = {
  type t = int;
  let empty = 0;
  let compare = Pervasives.compare;
};
module IntState = ChoosableOfBaseOrderedType(Int);

module type Patch = {
  type t;
  type id;
  type mutation;
  let replica: id => t;
  let join: (t, t) => t;
};

module Make = (Id: Map.OrderedType, State: JoinableState) => {
  type id = Id.t;
  type t = {
    id: option(id),
    state: State.t,
  };
  type mutation =
    | Result{
        replica: t,
        delta: t,
      }
    | Invalid{
        replica: t,
        delta: option(t),
      };

  let replica = id => {id, state: State.empty};

  let join = (p, q) => {
    let id =
      switch (p.id, q.id) {
      | (None, Some(id))
      | (Some(id), None) => Some(id)
      | (None, None) => None
      | (Some(id), Some(_)) => Some(id)
      };
    let state = State.join(p.state, q.state);
    {id, state};
  };

  let patchOfState = (~id=None, state: State.t) => {id, state};

  let mutate = (replica: t, deltaMutation: State.t) => {
    let delta = patchOfState(deltaMutation);
    switch (replica.id) {
    | Some(_) => Result({replica: join(replica, delta), delta})
    | _ => Invalid({replica, delta: Some(delta)})
    };
  };
};

module Map = (Key: OrderedState, Value: JoinableState) => {
  module Data = Map.Make(Key);

  type t = Data.t(Value.t);
  let empty = Data.empty;

  let join = (m, m') =>
    Data.merge(
      (_, mState, mState') =>
        switch (mState, mState') {
        | (Some(x), Some(y)) => Some(Value.join(x, y))
        | (Some(x), None)
        | (None, Some(x)) => Some(Value.join(x, Value.empty))
        | _ => None
        },
      m,
      m',
    );
};

module SimpleMap = (Key: BaseOrderedType, Value: JoinableState) =>
  Map((ChoosableOfBaseOrderedType(Key)), Value);
module Pair = (A: JoinableState, B: JoinableState) => {
  type t = (A.t, B.t);
  let empty = (A.empty, B.empty);
  let join = ((a, b), (a', b')) => (A.join(a, a'), B.join(b, b'));
};

module LexicographicPair = (A: ChoosableState, B: JoinableState) => {
  type t = (A.t, B.t);
  let empty = (A.empty, B.empty);
  let join = ((a, b), (a', b')) =>
    switch (A.choose(a, a')) {
    | Left(a) => (a, b)
    | Right(a') => (a', b')
    | Either(a, _) => (a, B.join(b, b'))
    | Neither(a, a') => (A.join(a, a'), B.empty)
    };
};
