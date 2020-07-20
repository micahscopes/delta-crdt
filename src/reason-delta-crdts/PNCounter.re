module Make = (Id: Map.OrderedType) => {
  module Data = Map.Make(Id);
  module Positive = GCounter.State(Data);
  module Negative = GCounter.State(Data);
  module State = Crdt.Pair(Positive, Negative);
  include Crdt.Make(Id, State);

  let replica = id => {id: Some(id), state: State.empty};
  let value = ({state: (positive, negative), _}) =>
    Positive.value(positive) - Negative.value(negative);

  let increment = replica =>
    switch (replica) {
    | {id: Some(id), state: (positive, _)} =>
      (Positive.increment(positive, id), Negative.empty)
      |> deltaOfState
      |> mutate(replica)
    | {id: None, _} => Invalid({replica, delta: None})
    };

  let decrement = replica =>
    switch (replica) {
    | {id: Some(id), state: (_, negative)} =>
      (Positive.empty, Negative.increment(negative, id))
      |> deltaOfState
      |> mutate(replica)
    | {id: None, _} => Invalid({replica, delta: None})
    };
};
