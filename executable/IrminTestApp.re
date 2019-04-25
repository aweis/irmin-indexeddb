print_endline("GOODBYE Hello! Ricky foobar hey there irminpack");

let () =
  Irmin_test.Store.run(
    "irmin",
    ~misc=[Test_pack.misc],
    [(`Quick, Test_pack.suite)],
  );
