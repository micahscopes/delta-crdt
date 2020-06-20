module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  include State
  
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
}