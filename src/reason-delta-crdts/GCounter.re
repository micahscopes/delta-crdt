module State = (Data: Map.S) => {
  open Data;
  type t = Data.t(int);
  let empty = Data.empty;
  let join = (p, q) => Data.merge(_ => max, p, q);
  let value = state => fold((_, v, accum) => v + accum, state, 0);
  let increment = (state, id) => empty |> add(id, find(id, state) + 1);
};

module Make = (Id: Map.OrderedType) => {
  module Data = Map.Make(Id);
  open Data;
  module State = State(Data);
  include Crdt.Make(Id, State);

  let initialValue = 0;
  let replica = id => {
    id: Some(id),
    state: State.empty |> add(id, initialValue),
  };

  let value = patch => State.value(patch.state);

  let increment = replica =>
    switch (replica) {
    | {id: Some(id), state} => State.increment(state, id) |> mutate(replica)
    | {id: None, _} => Invalid({replica, delta: None})
    };
};
