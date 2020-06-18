module Make = (Id: Map.OrderedType) => {
  module State = Map.Make(Id);
  open State;

  type state = State.t(int);
  type replica = Replica(Id.t, state);
  type delta = Delta(state);
  type joinable('a) =
    | Replica(Id.t, state): joinable(replica)  
    | Delta(state): joinable(delta)
  
  let stateOfJoinable: type a. joinable(a) => state =
    fun 
      | Replica(_, state) => state
      | Delta(state) => state; 

  let replica = id => Replica(id, empty |> add(id, 0));
  
  let value = (j: joinable('a)) =>
    fold((_,v,accum) => v+accum, stateOfJoinable(j), 0); 
    
  let increment = replica => {
    let Replica(id, state) = replica;
    let newValue = find(id, state) + 1;
    let delta = Delta(empty |> add(id, newValue));
    let state = state |> add(id, newValue);
    (Replica(id, state), delta);
  };

  let join: type a b. (joinable(a), joinable(b)) =>
    joinable(a) = (joinableA, joinableB) => {
      let mergedState = merge(
        (_) => max,
          stateOfJoinable(joinableA),
          stateOfJoinable(joinableB)
        );
      switch joinableA {
        | Replica(id, _) => Replica(id, mergedState)
        | Delta(_) => Delta(mergedState)
    }
  }
}
