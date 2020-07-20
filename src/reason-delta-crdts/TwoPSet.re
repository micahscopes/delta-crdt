module State = (Element: Set.OrderedType) => {
  module Data = Set.Make(Element);
  module AddSet = GSet.State(Data);
  module RemoveSet = GSet.State(Data);
  include Crdt.Pair(AddSet, RemoveSet);

  let insert = element => (AddSet.insert(element), RemoveSet.empty);
  let remove = element => (AddSet.empty, RemoveSet.insert(element));

  let elements = ((addSet, removeSet)) => Data.diff(addSet, removeSet);
};

module Make = (Id: Set.OrderedType, Element: Set.OrderedType) => {
  module State = State(Element);
  include Crdt.Make(Id, State);

  let replica = id => {id: Some(id), state: State.empty};
  let elements = ({id: _, state}) => state |> State.elements;

  let insert = (patch, element) => State.insert(element) |> mutate(patch);
  let remove = (patch, element) => State.remove(element) |> mutate(patch);
};
