module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  include CrdtCore.Make(State);
  open State;

  type id = Id.t
  type state = State.t(int);

  let value = patch => fold((_,v,accum) => v+accum, patch.state, 0); 
  
  let replica = replicaGenerator(0)

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