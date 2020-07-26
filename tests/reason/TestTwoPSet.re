open Tape;

test("test that we can add and remove an element only once", t => {
  module T = TwoPSet.Make(String, String);
  module Data = T.State.Data;
  open T;
  let marge = replica("marge");
  let el = "lol";
  let elSet = Data.singleton(el);

  let Result({replica: marge, _}) = insert(marge, el);
  Data.equal(elSet, elements(marge)) |> t.ok;

  let Result({replica: marge, _}) = insert(marge, el);
  Data.equal(elSet, elements(marge)) |> t.ok;

  let Result({replica: marge, _}) = remove(marge, el);
  Data.equal(Data.empty, elements(marge)) |> t.ok;

  let Result({replica: marge, _}) = remove(marge, el);
  Data.equal(Data.empty, elements(marge)) |> t.ok;

  let Result({replica: marge, _}) = insert(marge, el);
  Data.equal(Data.empty, elements(marge)) |> t.ok;

  t.endTest();
});
