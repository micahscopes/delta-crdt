module State = (Id: Set.OrderedType) => {
  module DotType = {
    type t = (Id.t, int);
    let compare = ((id, counter), (id', counter')) =>
      switch (Id.compare(id, id')) {
      | 0 => Pervasives.compare(counter, counter')
      | c => c
      };
  };
  module Data = Set.Make(DotType);
  include GSet.State(Data);

  let max = (state, id) => {
    let (_, max) =
      Data.filter(((id', _)) => id == id', state) |> Data.max_elt;
    max;
  };

  let next = (state, id) => (id, max(state, id) + 1)
};
