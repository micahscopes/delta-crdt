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
    let delta = deltaOfState(State.empty |> add(element))
    mutate(replica, delta)
  };
}