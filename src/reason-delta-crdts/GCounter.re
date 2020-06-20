module Make = (Id: Map.OrderedType) => {
  include CrdtCore.Make(Id)

  let replica = (id) => {id: Some(id), state: empty |> add(id, 0)};

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