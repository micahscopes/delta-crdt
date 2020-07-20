module type InOrOut = {
  include Crdt.JoinableState;
  let isIn: t;
  let isOut: t;
};

module Make =
       (
         Id: Map.OrderedType,
         Element: Map.OrderedType,
         Timestamp: Crdt.ComparableState,
         InOrOut: InOrOut,
       ) => {
  module LexState = Crdt.LexicographicPair(Timestamp, InOrOut);
  module Data = Map.Make(Element);
  module State = {
    type t = Data.t(LexState.t);
    let empty = Data.empty;
    let insert = (element, time) =>
      Data.add(element, (time, InOrOut.isIn), empty);
    let remove = (element, time) =>
      Data.add(element, (time, InOrOut.isOut), empty);
    let join = (m, m') =>
      Data.merge(
        (_, mState, mState') =>
          switch (mState, mState') {
          | (Some(x), Some(y)) => Some(LexState.join(x, y))
          | (Some(x), None)
          | (None, Some(x)) => Some(LexState.join(x, LexState.empty))
          | _ => None
          },
        m,
        m',
      );
  };

  include Crdt.Make(Id, State);

  module ElSet = Set.Make(Element);
  let elements = m =>
    Data.fold(
      (key, (_, isIn), elSet) =>
        switch (isIn) {
        | true => ElSet.add(key, elSet)
        | _ => elSet
        },
      m,
      ElSet.empty,
    );

  let insert = (replica, element, time) =>
    State.insert(element, time) |> deltaOfState |> mutate(replica);
  let remove = (replica, element, time) =>
    State.remove(element, time) |> deltaOfState |> mutate(replica);
};

module InOrOut = {
  type t = bool;
  let empty = false;
  let isOut = false;
  let isIn = true;
};

module IsInAddStays = {
  include InOrOut;
  let join = (a, b) => a || b;
};

module IsInRemoveStays = {
  include InOrOut;
  let join = (a, b) => a && b;
};

module AddWins =
       (
         Id: Map.OrderedType,
         Element: Map.OrderedType,
         Timestamp: Crdt.ComparableState,
       ) => {
  module Make = Make(Id, Element, Timestamp, IsInAddStays);
};

module RemoveWins =
       (
         Id: Map.OrderedType,
         Element: Map.OrderedType,
         Timestamp: Crdt.ComparableState,
       ) => {
  module Make = Make(Id, Element, Timestamp, IsInRemoveStays);
};
