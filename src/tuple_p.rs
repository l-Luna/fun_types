// A component of a projective tuple.
pub trait TupleC{
    type Mapped<F: TyFamily>: TupleC;
    type MappedPlus<'a, F: TyPlusFamily>: TupleC;
}

pub trait TyFamily{
    type App<T>;
}

pub trait TyPlusFamily{
    type App<'a, T>;
}

pub struct TNil;
pub struct TCons<X, Xs: TupleC>(X, Xs);

impl TupleC for TNil{
    type Mapped<F: TyFamily> = TNil;
    type MappedPlus<'a, F: TyPlusFamily> = TNil;
}
impl<X, Xs: TupleC> TupleC for TCons<X, Xs>{
    type Mapped<F: TyFamily> = TCons<F::App<X>, Xs::Mapped<F>>;
    type MappedPlus<'a, F: TyPlusFamily> = TCons<F::App<'a, X>, Xs::MappedPlus<'a, F>>;
}

// e.g.
type Ints = TCons<i32, TCons<i32, TNil>>;

#[allow(non_snake_case)] // type macro
macro_rules! TupleP{
    () => { TNil };
    ($x:ty) => { TCons<$x, TNil> };
    ($x:ty $(, $r:ty )* $(,)?) => { TCons<$x, TupleP!( $($r ,)* )> };
}

macro_rules! tup{
    () => { TNil };
    ($x:expr) => { TCons(x, TNil) };
    ($x:expr $(, $r:expr )* $(,)?) => { TCons($x, tup!( $($r ,)* )) };
}

// e.g.
type Ints_ = TupleP!(i32, i32);

// e.g.
pub struct Pairs;
impl TyFamily for Pairs{
    type App<T> = (T, T);
}

pub fn double<T: Copy>(x: T) -> <TupleP!(T, T, T) as TupleC>::Mapped<Pairs>{
    tup!((x, x), (x, x), (x, x))
}