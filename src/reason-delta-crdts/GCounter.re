module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  module Patch {
    type t(_, 'a) = State.t('a)
    let join = (p,q) => State.merge((_) => max, p, q)
    let empty = State.empty 
  }
  
  include Crdt.Make(Patch)
  open State

  let initialValue = 0
  let replica = id => {id: Some(id), state: Patch.empty |> add(id, initialValue)};

  let value = patch => fold((_,v,accum) => v+accum, patch.state, 0); 

  let increment = replica => {
    switch replica {
      | {id: Some(id), state } => {
        let newValue = find(id, state) + 1;
        let delta = empty |> add(id, newValue);
        let state = state |> add(id, newValue);
        Result({ replica: {id: Some(id), state}, delta: {id: None, state: delta} })
      }
      | {id: None, _ } => Invalid(replica)
    }
  };
}