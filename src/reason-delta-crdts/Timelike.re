/*
   Here is where the causal context stuff will go,
   along with other various clock-like things.
   For now it is filled with cruft.
 */

module type TimelikeState = {
  include Crdt.ChoosableState;
  let now: (~thePast: 'ctx, unit) => t;
};

module TimelikeCounterState = {
  module Int = {
    type t = int;
    let empty = 0;
    let compare = Pervasives.compare;
  };

  include Crdt.ChoosableOfBaseOrderedType(Int);
};
