open Tape

test("test that 14 is larger than 8", t => {
  let testResult = 14 > 8;
  t.plan(1);
  t.ok(testResult);
});
