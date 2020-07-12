module Make = (ElementType: Set.OrderedType) => {
  module State = Set.Make(ElementType);
  module Patch {
    type t(_, _) = State.t
    let join = (p,q) => State.union(p, q)
    let empty = State.empty 
  }
  
  module Core = Crdt.Make(Patch)
  include Core
  open State

  let replica = id => {id: Some(id), state: Patch.empty};

  let elements = patch => patch.state; 

  let insert = (replica, element) => {
    switch replica {
      | {id: Some(id), state } => {
        let delta = empty |> add(element);
        let state = state |> add(element);
        Result({ replica: {id: Some(id), state}, delta: {id: None, state: delta} })
      }
      | {id: None, _ } => Invalid(replica)
    }
  };
}