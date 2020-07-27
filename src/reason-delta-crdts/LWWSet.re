module type InOrOut = {
  type t = bool;
  let join: (t, t) => t;
  let empty: t;
  let isIn: t;
  let isOut: t;
};

module State =
       (
         Element: Crdt.BaseOrderedType,
         Timestamp: Crdt.ChoosableState,
         InOrOut: InOrOut,
       ) => {
  module LexState = Crdt.LexicographicPair(Timestamp, InOrOut);
  include Crdt.SimpleMap(Element, LexState);
  let insert = (element, time) =>
    Data.add(element, (time, InOrOut.isIn), empty);
  let remove = (element, time) =>
    Data.add(element, (time, InOrOut.isOut), empty);

  module ElSet = Set.Make(Element);
  let elements = m =>
    Data.fold(
      (elementKey, (_, isIn), elements) =>
        switch (isIn) {
        | true => ElSet.add(elementKey, elements)
        | _ => elements
        },
      m,
      ElSet.empty,
    );
};

module Make =
       (
         Id: Map.OrderedType,
         Element: Crdt.BaseOrderedType,
         Timestamp: Crdt.ChoosableState,
         InOrOut: InOrOut,
       ) => {
  module LexState = Crdt.LexicographicPair(Timestamp, InOrOut);
  module Data = Map.Make(Element);
  module State = State(Element, Timestamp, InOrOut);
  include Crdt.Make(Id, State);

  let elements = ({state, _}) => State.elements(state);
  let insert = (replica, element, time) =>
    State.insert(element, time) |> mutate(replica);
  let remove = (replica, element, time) =>
    State.remove(element, time) |> mutate(replica);
};

module InOrOut = {
  type t = bool;
  let empty = false;
  let isOut = false;
  let isIn = true;
};

module InOrOutAddStays = {
  include InOrOut;
  let join = (a, b) => a || b;
};

module InOrOutRemoveStays = {
  include InOrOut;
  let join = (a, b) => a && b;
};

module AddWins =
       (
         Id: Map.OrderedType,
         Element: Crdt.BaseOrderedType,
         Timestamp: Crdt.ChoosableState,
       ) => {
  module Make = Make(Id, Element, Timestamp, InOrOutAddStays);
};

module RemoveWins =
       (
         Id: Map.OrderedType,
         Element: Crdt.BaseOrderedType,
         Timestamp: Crdt.ChoosableState,
       ) => {
  module Make = Make(Id, Element, Timestamp, InOrOutRemoveStays);
};
