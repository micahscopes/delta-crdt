open Tape

test("test that we can increment the counter", t => {
  module CounterType = GCounter.Make(String);
  open CounterType;
  let (replica, dReplica) = increment(CounterType.replica("micah"));
  let replicaValue = value(replica);
  t.plan(1);
  t.ok(replicaValue === 1);
});

test("test that we can join deltas/replicas", t => {
  module CounterType = GCounter.Make(String);
  open CounterType;
  let alice = CounterType.replica("alice");
  let bob = CounterType.replica("bob");
  let marge = CounterType.replica("marge");

  let (alice, dAlice) = increment(alice);
  let (bob, dBob) = increment(bob);
  
  t.ok(value(join(alice, dBob)) === value(join(bob, dAlice)));
  t.ok(value(join(alice, dBob)) === value(join(alice, bob)));

  let delta = join(dAlice, dBob);
  let marge = join(marge, delta);

  t.ok(value(marge) === value(join(alice,bob)));

  t.endTest()
});
