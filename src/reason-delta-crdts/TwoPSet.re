module Make = (Id: Set.OrderedType, Element: Set.OrderedType) => {
  module Data = Set.Make(Element)  
  module AddSet = GSet.State(Data)
  module RemoveSet = GSet.State(Data)
  module State = Crdt.Pair(AddSet, RemoveSet)  
  open Data
  include Crdt.Make(Id, State)

  let replica = id => {id: Some(id), state: State.empty};

  let elements = ({id: _, state: (addSet, removeSet)}) =>
    diff(addSet, removeSet) |> elements;
  
  let insert = (patch, element) => {
    let delta = deltaOfState((empty |> add(element), empty))
    mutate(patch, delta)
  }
  
  let remove = (patch, element) => {
    let delta = deltaOfState((empty, empty |> add(element)))
    mutate(patch, delta) 
  }

}