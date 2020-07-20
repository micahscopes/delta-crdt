/*
   Here is where the causal context stuff will go,
   along with other various clock-like things.
   For now it is filled with cruft.
 */

module type TimelikeState = {
  include Crdt.ComparableState;
  let now: (~thePast: 'ctx, unit) => t;
};

module TimelikeCounterState = {
  module Int = {
    type t = int;
    let compare = Pervasives.compare;
  };

  include Crdt.ComparableOfOrderedType(Int);
  type t = Int.t;
  let empty = 0;

  let join = (t, t') =>
    switch (compare(t, t')) {
    | Either(x, _)
    | Left(x)
    | Right(x) => x
    | _ => empty
    };
};
