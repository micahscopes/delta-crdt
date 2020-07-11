module Make = (State: Map.S) => {
  open State;
  
  type patchType('a, 'b) = {id: option('a), state: 'b};

  type mutationType('patch) = Result{
    replica: 'patch,
    delta: 'patch
  } | Invalid('patch);

  let replicaGenerator = init => id => {id: Some(id), state: empty |> add(id, init)};
  
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
}