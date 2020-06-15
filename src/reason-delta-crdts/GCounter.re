module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  type state('a) = State.t('a);
  type replica('a) = Replica(Id.t, state('a));
  type delta('a) = Delta(state('a));
  type joinable('a) =
    | Replica(Id.t, state('a))
    | Delta(state('a));

  open State;
  let replica = id => Replica(id, empty |> add(id, 0));
  let value = (Replica(_, state)) =>
    fold((k,v,accum) => v+accum, state, 0); 
  let increment = (Replica(id, state)) => {
    let newValue = find(id, state) + 1;
    let delta = Delta(empty |> add(id, newValue));
    let state = state |> add(id, newValue);
    (Replica(id, state), delta);
  };

  let stateOfJoinable = j => {
    switch j {
      | Replica(_, state) => state;
      | Delta(state) => state; 
    }
  }

  let join = (Replica(id, state), joinable) =>
    Replica(id, merge((_) => max, state, stateOfJoinable(joinable)));
}
