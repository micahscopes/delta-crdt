module type JoinableState = {
  type t;
  let empty: t;
  let join: (t, t) => t;
};

module type Comparable = {
  type result('x) =
    | Left('x)
    | Either('x, 'x)
    | Neither('x, 'x)
    | Right('x);

  let compare: ('x, 'x) => result('x);
};

module ComparableOfOrderedType = (T: Map.OrderedType) => {
  type result('t) =
    | Left('t)
    | Right('t)
    | Either('t, 't)
    | Neither('t, 't);

  let compare: (T.t, T.t) => result(T.t) =
    (t, t') => {
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
};

module type ComparableState = {
  include JoinableState;
  include Comparable;
};

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
  let deltaOfState = (state: State.t) => {id: None, state};

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

  let mutate = (replica, delta) =>
    switch (replica.id, delta.id) {
    | (Some(_), None) => Result({replica: join(replica, delta), delta})
    | _ => Invalid({replica, delta: Some(delta)})
    };
};

module Pair = (A: JoinableState, B: JoinableState) => {
  type t = (A.t, B.t);
  let empty = (A.empty, B.empty);
  let join = ((a, b), (a', b')) => (A.join(a, a'), B.join(b, b'));
};

module LexicographicPair = (A: ComparableState, B: JoinableState) => {
  type t = (A.t, B.t);
  let empty = (A.empty, B.empty);
  let join = ((a, b), (a', b')) => {
    let priority = A.compare(a, a');
    switch (priority) {
    | Left(a) => (a, b)
    | Right(a') => (a', b')
    | Either(a, _) => (a, B.join(b, b'))
    | Neither(a, a') => (A.join(a, a'), B.empty)
    };
  };
};
