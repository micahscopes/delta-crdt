module type JoinableState {
  type t 
  let empty: t
  let join: (t,t) => t 
}

module type ComparableState {
  include JoinableState;
  type result = Left | Both | Neither | Right;
  
  let compare: (t,t) => result
}

module type Patch {
  type t
  type id
  let replica: id => t
  type mutation = Result{
    replica: t,
    delta: t
  } | Invalid(t);  
}

module Make = (Id: Map.OrderedType, State: JoinableState) => {
  type id = Id.t   
  type t = {id: option(id), state: State.t};
  type mutation = Result{
    replica: t,
    delta: t
  } | Invalid(t);  

  let replica = id => {id, state: State.empty} 

  let join = (p, q) => {
      let state = State.join(p.state, q.state);
      switch (p, q) {
        | ({id: None, _}, {id: Some(id), _})
        | ({id: Some(id), _}, {id: None, _}) => {id: Some(id), state}
        | ({id: None, _}, {id: None, _}) => {id: None, state}
        | ({id: Some(id)}, {id: Some(_)}) => {id: Some(id), state}
    }
  }
}

module Pair = (A: JoinableState, B: JoinableState) => {
  type t = (A.t, B.t)
  let empty = (A.empty, B.empty)
  let join = ((a, b), (a', b')) => (A.join(a, a'), B.join(b, b')) 
}

module LexicographicPair = (A: ComparableState, B: JoinableState) => {
  let empty = (A.empty, B.empty)
  let join = ((a,b), (a',b')) => {
    let priority = A.compare(a, a')
    switch (priority) {
      | Left => (a, b)
      | Right => (a', b')
      | Both => (a, B.join(b, b'))
      | Neither => (A.join(a, a'), B.empty)
    }
  } 
}  