module type Patch {
  type t('id, 'state)
  let empty: t('id, 'state)
  let join: (t('id, 'state), t('id, 'state)) => t('id, 'state);
}

module type ComparablePatch {
  include Patch
  type result = Left | Both | Neither | Right;
  
  let compare: (t('id, 'state), t('id, 'state)) => result
}

module Make = (Patch: Patch) => {
  type t('id, 'state) = {id: option('id), state: 'state};

  type mutationType('patch) = Result{
    replica: 'patch,
    delta: 'patch
  } | Invalid('patch);
 
  let join = (p, q) => {
      let state = Patch.join(p.state, q.state);
      switch (p, q) {
        | ({id: None, _}, {id: Some(id), _})
        | ({id: Some(id), _}, {id: None, _}) => {id: Some(id), state}
        | ({id: None, _}, {id: None, _}) => {id: None, state}
        | ({id: Some(id)}, {id: Some(_)}) => {id: Some(id), state}
    }
  }
}

module Pair = (A: Patch, B: Patch) => {
  module Patch {
    type t('id, 'state) = (A.t('id, 'state), B.t('id, 'state))
    let empty = (A.empty, B.empty)
    let join = ((a, b), (a', b')) => (A.join(a, a'), B.join(b, b')) 
  } 
}

module LexicographicPair = (A: ComparablePatch, B: Patch) => {
  module Patch {
    type t('id, 'state) = (A.t('id, 'state), B.t('id, 'state))
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
}