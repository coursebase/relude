module IO = Relude_IO;

module WithBiFunctor = (B: BsAbstract.Interface.BIFUNCTOR) => {
  type t('r, 'a, 'e) =
    | ReaderT('r => B.t('a, 'e));

  let bimap:
    'r 'a 'b 'e 'f.
    ('a => 'b, 'e => 'f, t('r, 'a, 'e)) => t('r, 'b, 'f)
   =
    (aToB, eToF, ReaderT(rToBiFunctor)) =>
      ReaderT(r => B.bimap(aToB, eToF, rToBiFunctor(r)));

  let map = (aToB, ReaderT(rToBiFunctor)) =>
    ReaderT(r => B.bimap(aToB, e => e, rToBiFunctor(r)));

  let make: 'r 'a 'e. ('r => B.t('a, 'e)) => t('r, 'a, 'e) =
    rToBAE => ReaderT(rToBAE);

  let runReaderT: 'r 'a 'e. ('r, t('r, 'a, 'e)) => B.t('a, 'e) =
    (r, ReaderT(rToMA)) => rToMA(r);

  module WithEnv = (R: BsAbstract.Interface.TYPE) => {
    type nonrec t('a, 'e) = t(R.t, 'a, 'e);

    let make = make;
    let runReaderT = runReaderT;

    module BiFunctor:
      BsAbstract.Interface.BIFUNCTOR with type t('a, 'e) = t('a, 'e) = {
      type nonrec t('a, 'e) = t('a, 'e);
      let bimap = bimap;
    };
    let map = map;

    include Relude_Extensions_Bifunctor.BifunctorExtensions(BiFunctor);

    module Infix = {
      include Relude_Extensions_Bifunctor.BifunctorInfix(BiFunctor);
    };
  };
};

module WithBiFunctorAndEnv =
       (BF: BsAbstract.Interface.BIFUNCTOR, E: BsAbstract.Interface.TYPE) => {
  module WithBiFunctor = WithBiFunctor(BF);
  include WithBiFunctor.WithEnv(E);
};

module Make = (ERR: BsAbstract.Interface.TYPE, ENV: BsAbstract.Interface.TYPE) => {
  module IOE =
    IO.WithError({
      type t = ERR.t;
    });
  include WithBiFunctorAndEnv(IOE.Bifunctor, ENV);
};