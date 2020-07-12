module State = (Data: Set.S) => {
  type t = Data.t
  let empty = Data.empty
  let join = (p,q) => Data.union(p,q)
}

module Make = (Id: Set.OrderedType, Element: Set.OrderedType) => {
  module Data = Set.Make(Element);
  open Data
  module State = State(Data)   
  include Crdt.Make(Id, State)

  let replica = id => {id: Some(id), state: empty};

  let elements = patch => patch.state; 

  let insert = (replica, element) => {
    switch replica {
      | {id: Some(id), state } => {
        let delta = State.empty |> add(element);
        let state = state |> add(element);
        Result({ replica: {id: Some(id), state}, delta: {id: None, state: delta} })
      }
      | {id: None, _ } => Invalid(replica)
    }
  };
}