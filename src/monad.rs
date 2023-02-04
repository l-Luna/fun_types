use std::marker::PhantomData;

// better idea; none of that plug/unplug nonsense, just define everything on the type family
// also! use FnOnce to make everything far easier...

trait Monad{
    type App<T>;

    fn pure<T>(x: T) -> Self::App<T>;

    fn bind_ref<'a, T: 'a, U>(x: &'a Self::App<T>, f: impl Fn(&'a T) -> Self::App<U>) -> Self::App<U>;
}

trait MonadOnce: Monad{
    fn bind_once<T, U>(x: Self::App<T>, f: impl FnOnce(T) -> Self::App<U>) -> Self::App<U>;
}

trait MonadDyn: Monad{
    type AppDyn<T: ?Sized>;

    fn bind_dyn<'a, T: 'a + ?Sized, U: ?Sized>(x: &'a Self::AppDyn<T>, f: impl Fn(&'a T) -> Self::AppDyn<U>) -> Self::AppDyn<U>;
}

// implementations

struct Options;
impl Monad for Options{
    type App<T> = Option<T>;

    fn pure<T>(x: T) -> Option<T>{
        Some(x)
    }

    fn bind_ref<'a, T: 'a, U>(x: &'a Option<T>, f: impl Fn(&'a T) -> Option<U>) -> Option<U>{
        f(x.as_ref()?)
    }
}
impl MonadOnce for Options{
    fn bind_once<T, U>(x: Option<T>, f: impl FnOnce(T) -> Option<U>) -> Option<U>{
        f(x?)
    }
}

struct Results<E: Clone>(PhantomData<E>);
impl<E: Clone> Monad for Results<E>{
    type App<T> = Result<T, E>;

    fn pure<T>(x: T) -> Result<T, E>{
        Ok(x)
    }

    fn bind_ref<'a, T: 'a, U>(x: &'a Result<T, E>, f: impl Fn(&'a T) -> Result<U, E>) -> Result<U, E>{
        match x{
            Ok(x) => f(x),
            Err(e) => Err(e.clone())
        }
    }
}
impl<E: Clone> MonadOnce for Results<E>{
    fn bind_once<T, U>(x: Result<T, E>, f: impl FnOnce(T) -> Result<U, E>) -> Result<U, E>{
        f(x?)
    }
}

struct Vecs;
impl Monad for Vecs{
    type App<T> = Vec<T>;

    fn pure<T>(x: T) -> Vec<T>{
        vec![x]
    }

    fn bind_ref<'a, T: 'a, U>(xs: &'a Vec<T>, f: impl Fn(&'a T) -> Vec<U>) -> Vec<U>{
        let mut tmp: Vec<U> = vec![];
        for x in xs{
            tmp.extend(f(x));
        }
        return tmp;
    }
}

// not super useful...
struct Boxes;
impl Monad for Boxes{
    type App<T> = Box<T>;

    fn pure<T>(x: T) -> Box<T>{
        Box::new(x)
    }

    fn bind_ref<'a, T: 'a, U>(x: &'a Box<T>, f: impl Fn(&'a T) -> Box<U>) -> Box<U>{
        f(x.as_ref())
    }
}
impl MonadOnce for Boxes{
    fn bind_once<T, U>(x: Box<T>, f: impl FnOnce(T) -> Box<U>) -> Box<U> {
        f(*x)
    }
}
// ...most of the time
impl MonadDyn for Boxes{
    type AppDyn<T: ?Sized> = Box<T>;

    fn bind_dyn<'a, T: 'a + ?Sized, U: ?Sized>(x: &'a Self::AppDyn<T>, f: impl Fn(&'a T) -> Self::AppDyn<U>) -> Self::AppDyn<U> {
        f(x.as_ref())
    }
}

// usage!

fn pairs<'a, T, U, M: Monad>(l: &'a M::App<T>, r: &'a M::App<U>) -> M::App<(&'a T, &'a U)>{
    M::bind_ref(l, |x| M::bind_ref(r, |y| M::pure((x, y))))
}

// hey, it works!
// (arrow notation anyone...?)
macro_rules! mdo{
    ($t:ty | $i:ident <-- $e:expr; $($k:tt)*) => { <$t>::bind_ref($e, |$i| mdo!( $t | $($k)* ) ) };
    ($t:ty | $i:ident <~~ $e:expr; $($k:tt)*) => { <$t>::bind_once($e, |$i| mdo!( $t | $($k)* ) ) };
    ($t:ty | $i:ident <<- $e:expr; $($k:tt)*) => { <$t>::bind_dyn($e, |$i| mdo!( $t | $($k)* ) ) };
    ($t:ty | let $i:ident = $e:expr; $($k:tt)*) => { { let $i = $e; mdo!( $t | $($k)* ) } };
    ($t:ty | pure $e:expr) => { <$t>::pure($e) };
    ($t:ty | $e:expr) => { $e };
}

fn pairs_<'a, T, U, M: Monad>(l: &'a M::App<T>, r: &'a M::App<U>) -> M::App<(&'a T, &'a U)>{
    mdo!{ M |
        x <-- l;
        y <-- r;
        pure (x, y)
    }
}

fn pairs_own<T, U, M: MonadOnce>(l: M::App<T>, r: M::App<U>) -> M::App<(T, U, i32)>{
    mdo!{ M |
        x <~~ l;
        y <~~ r;
        let k = 1;
        pure (x, y, k)
    }
}

fn dsts(f: &Box<dyn Fn(i32) -> i32>, i: &Box<i32>, f2: Box<fn(i32) -> i32>) -> Box<i32>{
    mdo!{ Boxes |
        fxo <~~ f2;
        fx <<- f;
        ix <-- i;
        pure fxo(fx(*ix))
    }
}

#[test]
fn test(){
    println!("{:?}", pairs_::<i32,_,Options>(&None, &Some(2)));
}