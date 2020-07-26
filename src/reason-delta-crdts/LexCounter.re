module State = (Id: Crdt.BaseOrderedType) => {
  module LexState = {
    include Crdt.LexicographicPair(Crdt.IntState, Crdt.IntState);
    let increment = ((t, x)) => (t, x + 1);
    let decrement = ((t, x)) => (t + 1, x - 1);
  };
  module Map = Crdt.SimpleMap(Id, LexState);
  open Map.Data;
  include Map;

  let increment = (state, id) =>
    singleton(id, find(id, state) |> LexState.increment);
  let decrement = (state, id) =>
    singleton(id, find(id, state) |> LexState.decrement);

  let value = state => fold((_, (_, x), accum) => x + accum, state, 0);
};

module Make = (Id: Crdt.BaseOrderedType) => {
  module State = State(Id);
  include Crdt.Make(Id, State);

  let increment = ({state, id} as replica) =>
    switch (id) {
    | Some(id) => State.increment(state, id) |> mutate(replica)
    | _ => Invalid({replica, delta: None})
    };

  let decrement = ({state, id} as replica) =>
    switch (id) {
    | Some(id) => State.decrement(state, id) |> mutate(replica)
    | _ => Invalid({replica, delta: None})
    };

  let value = ({state, _}) => State.value(state);
};
