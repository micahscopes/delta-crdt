module State = (Id: Map.OrderedType) => {
  module Data = Map.Make(Id);
  module Positive = GCounter.State(Data);
  module Negative = GCounter.State(Data);
  include Crdt.Pair(Positive, Negative);

  let increment = (id, (positive, _)) => (
    Positive.increment(positive, id),
    Negative.empty,
  );
  let decrement = (id, (_, negative)) => (
    Positive.empty,
    Negative.increment(negative, id),
  );
  let value = ((positive, negative)) =>
    Positive.value(positive) - Negative.value(negative);
};

module Make = (Id: Map.OrderedType) => {
  module State = State(Id);
  include Crdt.Make(Id, State);

  let replica = id => {id: Some(id), state: State.empty};
  let value = ({state, _}) => State.value(state);

  let increment = ({id, state} as replica) =>
    switch (id) {
    | Some(id) => State.increment(id, state) |> mutate(replica)
    | None => Invalid({replica, delta: None})
    };

  let decrement = ({id, state} as replica) =>
    switch (id) {
    | Some(id) => State.decrement(id, state) |> mutate(replica)
    | None => Invalid({replica, delta: None})
    };
};
