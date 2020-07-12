open Tape

test("test that we can make a GSet and add an element", t => {
  module GSetType = GSet.Make(String);
  open GSetType;
  open GSetType.State

  let el = "hello"
  let expected = empty |> add(el)
  let replica = GSetType.replica("marge");
  let Result{replica} = insert(replica, el)
  t.ok(equal(GSetType.elements(replica), expected));
  t.endTest()
});

test("test that we can join deltas/replicas", t => {
  module GSetType = GSet.Make(String);
  open GSetType;
  let equal = GSetType.State.equal
  let alice = GSetType.replica("alice");
  let bob = GSetType.replica("bob");
  let marge = GSetType.replica("marge");

  let Result{ replica: alice, delta: dAlice } = insert(alice, "dog");
  let Result{ replica: bob, delta: dBob } = insert(bob, "cat");

  let checkEqualElements = (x, y) => equal(elements(x), elements(y));

  checkEqualElements(join(alice, dBob), join(bob, dAlice)) |> t.ok;
  checkEqualElements(join(alice, dBob), join(alice, bob)) |> t.ok;

  let delta = join(dAlice, dBob);
  let marge = join(marge, delta);

  checkEqualElements(marge, join(alice, bob)) |> t.ok;

  t.endTest()
});
