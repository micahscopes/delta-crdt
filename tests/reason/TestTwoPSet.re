
open Tape

test("test that we can add and remove an element only once", t => {
  module T = TwoPSet.Make(String, String);
  open T;
  let marge = replica("marge");
  let el = "lol"
  let elSet = T.Data.empty |> T.Data.add(el)
  
  let Result{replica: marge, _} = insert(marge, el);
  T.Data.equal(elSet, elements(marge)) |> t.ok;

  let Result{replica: marge, _} = insert(marge, el);
  T.Data.equal(elSet, elements(marge)) |> t.ok;
  
  let Result{replica: marge, _} = remove(marge, el)
  T.Data.equal(T.Data.empty, elements(marge)) |> t.ok;

  let Result{replica: marge, _} = remove(marge, el)
  T.Data.equal(T.Data.empty, elements(marge)) |> t.ok;
  
  t.endTest();
});