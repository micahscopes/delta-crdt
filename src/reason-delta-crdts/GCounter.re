module State = (Data: Map.S) => {
  type t = Data.t(int);
  let empty = Data.empty;
  let join = (p, q) => Data.merge(_ => max, p, q);
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

  let value = patch => fold((_, v, accum) => v + accum, patch.state, 0);

  let increment = replica =>
    switch (replica) {
    | {id: Some(id), state} =>
      let newCount = find(id, state) + 1;
      let delta = deltaOfState(State.empty |> add(id, newCount));
      mutate(replica, delta);
    | {id: None, _} => Invalid({replica, delta: None})
    };
};
