open Jest;
open Expect;

type env = {
  intValue: int,
  stringValue: string,
};

module Environment = {
  type t = env;
  module Type: BsAbstract.Interface.TYPE with type t = t = {
    type nonrec t = t;
  }
};
let testEnv: env = {intValue: 42, stringValue: "abc"};

type error = {message: string};

module Error = {
  type t = error;
  module Type: BsAbstract.Interface.TYPE with type t = t = {
    type nonrec t = t;
  }
};

module RIO = Relude.RIO;

module R = RIO.Make(Error, Environment);
module IOE = R.IOE;

describe("RIO", () => {
  testAsync("simple flow", onDone => {
    R.make(env => IOE.pure(-1 * env.intValue))
     |> R.map(a => a * 2)
     |> R.runReaderT(testEnv)
     |> IOE.Bifunctor.bimap(
      a => expect(a) |> toEqual(-84), 
      d=>d)
    |> Relude_IO.unsafeRunAsync(
         fun
         | Ok(assertion) => onDone(assertion)
         | Error(_) => onDone(fail("fail")),
       )
  })
  }
);