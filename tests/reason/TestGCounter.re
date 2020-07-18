open Tape;

test("test that we can increment the counter", t => {
  module CounterType = GCounter.Make(String);
  open CounterType;
  let Result({replica}) = increment(CounterType.replica("marge"));
  t.ok(value(replica) === 1);
  t.endTest();
});

test("test that we can join deltas/replicas", t => {
  module CounterType = GCounter.Make(String);
  open CounterType;
  let alice = CounterType.replica("alice");
  let bob = CounterType.replica("bob");
  let marge = CounterType.replica("marge");

  let Result({replica: alice, delta: dAlice}) = increment(alice);
  let Result({replica: bob, delta: dBob}) = increment(bob);

  t.ok(value(join(alice, dBob)) === value(join(bob, dAlice)));
  t.ok(value(join(alice, dBob)) === value(join(alice, bob)));

  let delta = join(dAlice, dBob);
  let marge = join(marge, delta);

  t.ok(value(marge) === value(join(alice, bob)));

  t.endTest();
});
