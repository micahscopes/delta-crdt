module type DotType = {
  type id;
  type timelike;
  let emptyTime: timelike;
  type t = (id, timelike);
  let next: timelike => timelike 
  let compare: ((id, timelike), (id, timelike)) => int;
};

module DotType = (Id: Set.OrderedType) => {
  type id = Id.t;
  type timelike = int;
  type t = (id, int);
  let emptyTime = 0;
  let next = i => i+1;
  let compare = ((id, counter), (id', counter')) =>
    switch (Id.compare(id, id')) {
    | 0 => Pervasives.compare(counter, counter')
    | c => c
    };
};

module type Context = {
  include Crdt.JoinableState;
  type id;
  type timelike;
  let max: (t, id) => timelike;
  let next: (t, id) => t;
};

module Context = (DotType: DotType) => {
  
  module Data = Set.Make(DotType);
  include GSet.State(Data);

  let max = (state, id) => {
    let (_, max) =
      Data.filter(((id', _)) => id == id', state)
      |> Data.add((id, DotType.emptyTime))
      |> Data.max_elt;
    max;
  };

  let next = (state, id) => (id, max(state, id) |> DotType.next)
};
