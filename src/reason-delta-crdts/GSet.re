module State = (Data: Set.S) => {
  type t = Data.t;
  let empty = Data.empty;
  let join = (p, q) => Data.union(p, q);
  let insert = element => empty |> Data.add(element);
};

module Make = (Id: Set.OrderedType, Element: Set.OrderedType) => {
  module Data = Set.Make(Element);
  open Data;
  module State = State(Data);
  include Crdt.Make(Id, State);

  let replica = id => {id: Some(id), state: empty};

  let elements = patch => patch.state;

  let insert = (replica, element) =>
    State.insert(element) |> mutate(replica);
};
