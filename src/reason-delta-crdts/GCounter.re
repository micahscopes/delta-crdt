module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  open State;

  type id = Id.t
  type state = State.t(int);
  type patch = {id: option(id), state: state};

  type mutation = Result{
    replica: patch,
    delta: patch
  } | Invalid(patch);
  
  let join = (p, q) => {
      let state = merge(
        (_) => max, p.state, q.state);
      switch (p, q) {
        | ({id: None, _}, {id: Some(id), _})
        | ({id: Some(id), _}, {id: None, _}) => {id: Some(id), state}
        | ({id: None, _}, {id: None, _}) => {id: None, state}
        | ({id: Some(id)}, {id: Some(_)}) => {id: Some(id), state}
    }
  }

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