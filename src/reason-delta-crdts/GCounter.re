module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  type replicaType = (Id.t, State.t(Id.t));
  open State;
  let replica = id => (id, empty |> add(id, 0));
  let value = ((_, state)) =>
    fold((k,v,accum) => v+accum, state, 0); 
  let increment = ((id, state)) => {
    let newValue = find(id, state) + 1;
    let delta = empty |> add(id, newValue);
    let replica = state |> add(id, newValue);
    ((id, replica), delta);
  };

  
  let join = ((id, state), delta) =>
    (id, merge((_) => max, state, delta));
  let join = ((id, stateA), (_, stateB)) =>
    (id, merge((_) => max, stateA, stateB));
}
