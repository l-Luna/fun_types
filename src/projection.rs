use std::marker::PhantomData;
use std::pin::Pin;

// idea: generify reference-ness
// &T/Fn, &mut T/FnMut, T/FnOnce, &T where T: Clone/FnOnce, Pin<T>/???

// A function generic over reference-ness.
// and by generic, i mean it has to be implemented for T, &mut T, and &T
// plus a macro that makes that (hopefully) easy

pub trait FnControl<I, Is>{
    type Output;

    fn call_mut<'s, 'a>(&'s self, input: &'a mut I, rest: Is) -> &'a mut Self::Output;
    fn call_own(&self, input: I, rest: Is) -> Self::Output;
}

pub struct ClosureControl<FM, FO, I, Is, O> where
    FM: Fn(&mut I, Is) -> &mut O,
    FO: Fn(I, Is) -> O
{
    by_mut: FM,
    by_own: FO,
    _mark: PhantomData<fn(I, Is) -> O> // variance
}

pub type FpControl<I, Is, O> = ClosureControl<fn(&mut I) -> &mut O, fn(I) -> O, I, Is, O>;

impl<FM, FO, I, Is, O> FnControl<I, Is> for ClosureControl<FM, FO, I, Is, O> where
    FM: Fn(&mut I, Is) -> &mut O,
    FO: Fn(I, Is) -> O
{
    type Output = O;

    fn call_mut<'s, 'a>(&'s self, input: &'a mut I, rest: Is) -> &'a mut O{
        (self.by_mut)(input, rest)
    }

    fn call_own(&self, input: I, rest: Is) -> O{
        (self.by_own)(input, rest)
    }
}

impl<I, Is, O> FnControl<I, Is> for &dyn FnControl<I, Is, Output = O>{
    type Output = O;
    fn call_mut<'s, 'a>(&'s self, input: &'a mut I, rest: Is) -> &'a mut O{
        <dyn FnControl<I, Is, Output = O>>::call_mut(self, input, rest)
    }
    fn call_own(&self, input: I, rest: Is) -> O{
        <dyn FnControl<I, Is, Output = O>>::call_own(self, input, rest)
    }
}

#[macro_export]
macro_rules! control_fn{
    (|$x:ident: $t:ty| $e:expr) => {
        ClosureControl{
            by_mut: |$x: &mut $t, _| &mut ($e),
            by_own: |$x: $t, _| $e,
            _mark: PhantomData
        }
    };
    // TODO
}

#[allow(non_snake_case)] // type macro; trait macros don't seem to be a thing that exists sadly
#[macro_export]
macro_rules! ImplFnControl{
    (=> $U:ty) => { impl FnControl<(), (), Output = $U> };
    ($T:ty => $U:ty) => { impl FnControl<$T, (), Output = $U> };
    ($T:ty, *$Ts:ty => $U:ty) => { impl FnControl<$T, $Ts, Output = $U> };
    ($T:ty $(, $Ts:ty)* => $U:ty) => { impl FnControl<$T, ($($Ts,)*), Output = $U> };
}

#[allow(non_snake_case)] // type macro; trait macros don't seem to be a thing that exists sadly
#[macro_export]
macro_rules! DynFnControl{
    (=> $U:ty) => { dyn FnControl<(), (), Output = $U> };
    ($T:ty => $U:ty) => { dyn FnControl<$T, (), Output = $U> };
    ($T:ty, *$Ts:ty => $U:ty) => { dyn FnControl<$T, $Ts, Output = $U> };
    ($T:ty $(, $Ts:ty)* => $U:ty) => { dyn FnControl<$T, ($($Ts,)*), Output = $U> };
}



pub trait FnView<I, Is>: FnControl<I, Is>{
    fn call_ref<'s, 'a>(&'s self, input: &'a I, rest: Is) -> &'a Self::Output;
}

pub struct ClosureView<FR, FM, FO, I, Is, O> where
    FR: Fn(&I, Is) -> &O,
    FM: Fn(&mut I, Is) -> &mut O,
    FO: Fn(I, Is) -> O
{
    control: ClosureControl<FM, FO, I, Is, O>,
    view: FR
}

pub type FpView<I, Is, O> = ClosureView<fn(&I, Is) -> &O, fn(&mut I, Is) -> &mut O, fn(I, Is) -> O, I, Is, O>;

impl<FR, FM, FO, I, Is, O> FnControl<I, Is> for ClosureView<FR, FM, FO, I, Is, O> where
    FM: Fn(&mut I, Is) -> &mut O,
    FO: Fn(I, Is) -> O,
    FR: Fn(&I, Is) -> &O
{
    type Output = O;

    fn call_mut<'s, 'a>(&'s self, input: &'a mut I, rest: Is) -> &'a mut O{
        self.control.call_mut(input, rest)
    }

    fn call_own(&self, input: I, rest: Is) -> O{
        self.control.call_own(input, rest)
    }
}

impl<FR, FM, FO, I, Is, O> FnView<I, Is> for ClosureView<FR, FM, FO, I, Is, O> where
    FR: Fn(&I, Is) -> &O,
    FM: Fn(&mut I, Is) -> &mut O,
    FO: Fn(I, Is) -> O
{
    fn call_ref<'s, 'a>(&'s self, input: &'a I, rest: Is) -> &'a O{
        (self.view)(input, rest)
    }
}

impl<I, Is, O> FnControl<I, Is> for &dyn FnView<I, Is, Output = O>{
    type Output = O;
    fn call_mut<'s, 'a>(&'s self, input: &'a mut I, rest: Is) -> &'a mut O{
        <dyn FnView<I, Is, Output = O>>::call_mut(self, input, rest)
    }
    fn call_own(&self, input: I, rest: Is) -> O{
        <dyn FnView<I, Is, Output = O>>::call_own(self, input, rest)
    }
}

impl<I, Is, O> FnView<I, Is> for &dyn FnView<I, Is, Output = O>{
    fn call_ref<'s, 'a>(&'s self, input: &'a I, rest: Is) -> &'a O{
        <dyn FnView<I, Is, Output = O>>::call_ref(self, input, rest)
    }
}

#[macro_export]
macro_rules! view_fn{
    (|$x:ident: $t:ty| $e:expr) => {
        ClosureView{
            view: |$x: &$t, _| &($e),
            control: control!(|$x: $t| $e)
        }
    };
    // TODO
}

#[allow(non_snake_case)] // type macro; trait macros don't seem to be a thing that exists sadly
#[macro_export]
macro_rules! ImplFnView{
    (=> $U:ty) => { impl FnView<(), (), Output = $U> };
    ($T:ty => $U:ty) => { impl FnView<$T, (), Output = $U> };
    ($T:ty, *$Ts:ty => $U:ty) => { impl FnView<$T, $Ts, Output = $U> };
    ($T:ty $(, $Ts:ty)* => $U:ty) => { impl FnView<$T, ($($Ts,)*), Output = $U> };
}

#[allow(non_snake_case)] // type macro; trait macros don't seem to be a thing that exists sadly
#[macro_export]
macro_rules! DynFnView{
    (=> $U:ty) => { dyn FnView<(), (), Output = $U> };
    ($T:ty => $U:ty) => { dyn FnView<$T, (), Output = $U> };
    ($T:ty, *$Ts:ty => $U:ty) => { dyn FnView<$T, $Ts, Output = $U> };
    ($T:ty $(, $Ts:ty)* => $U:ty) => { dyn FnView<$T, ($($Ts,)*), Output = $U> };
}

// TODO: FnView/FnControl composition

// A projection is a type of reference-ness.
// T, &mut T, and &T are simple projections; copyable and pinned data also act like projections.

pub trait Projection{
    type View<'a, T: 'a>;
    type Control<'a, T: 'a>;
    type Pass<T> /* = () */; // Cloned asks for a clone function, Pinned *should* ask for proof of structural pinning.

    //trait Fn; // ??? logically Own should get to use FnOnce, Mut should get FnMut, etc.

    fn apply_view<'a, T: 'a, U: 'a, Is>(x: Self::View<'a, T>, pass: Self::Pass<T>, rest: Is, f: ImplFnView!(T, *Is => U)) -> Self::View<'a, U>;

    fn apply_control<'a, T: 'a, U: 'a, Is>(x: Self::View<'a, T>, pass: Self::Pass<T>, rest: Is, f: ImplFnControl!(T, *Is => U)) -> Self::Control<'a, U>;
}

struct Own;
impl Projection for Own{
    type View<'a, T: 'a> = T;
    type Control<'a, T: 'a> = T;
    type Pass<T> = ();

    fn apply_view<'a, T: 'a, U: 'a, Is>(x: T, _: (), rest: Is, f: ImplFnView!(T, *Is => U)) -> U{
        f.call_own(x, rest)
    }

    fn apply_control<'a, T: 'a, U: 'a, Is>(x: T, _: (), rest: Is, f: ImplFnControl!(T, *Is => U)) -> U{
        f.call_own(x, rest)
    }
}

struct Mut;
impl Projection for Mut{
    type View<'a, T: 'a> = &'a mut T;
    type Control<'a, T: 'a> = &'a mut T;
    type Pass<T> = ();

    fn apply_view<'a, T: 'a, U: 'a, Is>(x: &'a mut T, _: (), rest: Is, f: ImplFnView!(T, *Is => U)) -> &'a mut U{
        f.call_mut(x, rest)
    }

    fn apply_control<'a, T: 'a, U: 'a, Is>(x: &'a mut T, _: (), rest: Is, f: ImplFnControl!(T, *Is => U)) -> &'a mut U{
        f.call_mut(x, rest)
    }
}

struct Ref;
impl Projection for Ref{
    type View<'a, T: 'a> = &'a T;
    type Control<'a, T: 'a> = ();
    type Pass<T> = ();

    fn apply_view<'a, T: 'a, U: 'a, Is>(x: &'a T, _: (), rest: Is, f: ImplFnView!(T, *Is => U)) -> &'a U{
        f.call_ref(x, rest)
    }

    fn apply_control<'a, T: 'a, U: 'a, Is>(_x: &'a T, _: (), _rest: Is, _f: ImplFnControl!(T, *Is => U)) -> (){
        ()
    }
}

struct Cloned;
impl Projection for Cloned{
    type View<'a, T: 'a> = &'a T;
    type Control<'a, T: 'a> = T;
    type Pass<T> = fn(&T) -> T;

    fn apply_view<'a, T: 'a, U: 'a, Is>(x: &'a T, _pass: fn(&T) -> T, rest: Is, f: ImplFnView!(T, *Is => U)) -> &'a U{
        f.call_ref(x, rest)
    }

    fn apply_control<'a, T: 'a, U: 'a, Is>(x: &'a T, pass: fn(&T) -> T, rest: Is, f: ImplFnControl!(T, *Is => U)) -> U{
        f.call_own(pass(x), rest)
    }
}

// i'm like 99% sure this is wrong, but it's the thought that counts!!!
// really you'd want two projections (structural/non-structural) controlled with clever Pass usage
struct Pinned;
impl Projection for Pinned{
    type View<'a, T: 'a> = Pin<&'a mut T>;
    type Control<'a, T: 'a> = Pin<&'a mut T>;
    type Pass<T> = ();

    fn apply_view<'a, T: 'a, U: 'a, Is>(x: Pin<&'a mut T>, _: (), rest: Is, f: ImplFnView!(T, *Is => U)) -> Pin<&'a mut U>{
        unsafe{
            x.map_unchecked_mut(|s| f.call_mut(s, rest))
        }
    }

    fn apply_control<'a, T: 'a, U: 'a, Is>(x: Pin<&'a mut T>, _: (), rest: Is, f: ImplFnControl!(T, *Is => U)) -> Pin<&'a mut U>{
        unsafe{
            x.map_unchecked_mut(|s| f.call_mut(s, rest))
        }
    }
}

// Lenses generic over reference-ness.

pub trait Lens<I, O>{
    fn view(&self) -> &DynFnView!(I => O); // sorry for the &dyn but we don't have ITIT yet
    fn control(&self) -> &DynFnControl!(I, *O => I);
}

// e.g.
pub fn get_ref_<I, O>(l: impl Lens<I, O>, x: &I) -> &O{
    l.view().call_ref(x, ())
}

pub fn set_mut_ref_<I, O>(l: impl Lens<I, O>, x: &mut I, e: O) -> &mut I{
    l.control().call_mut(x, e)
}

// and finally!
pub fn get_raw<'a, I, O, P: Projection>(l: impl Lens<I, O>, x: P::View<'a, I>, pass: P::Pass<I>) -> P::View<'a, O>{
    P::apply_view(x, pass, (), l.view())
}

pub fn set_raw<'a, I, O, P: Projection>(l: impl Lens<I, O>, x: P::View<'a, I>, e: O, pass: P::Pass<I>) -> P::Control<'a, I>{
    P::apply_control(x, pass, e, l.control())
}

// everything else derived from that
pub fn get<I, O>(l: impl Lens<I, O>, x: &I) -> &O{
    get_raw::<_,_,Ref>(l, x, ())
}

pub fn get_mut<I, O>(l: impl Lens<I, O>, x: &mut I) -> &mut O{
    get_raw::<_,_,Mut>(l, x, ())
}

pub fn get_own<I, O>(l: impl Lens<I, O>, x: I) -> O{
    get_raw::<_,_,Own>(l, x, ())
}

pub fn set_mut<I, O>(l: impl Lens<I, O>, x: &mut I, e: O) -> &mut I{
    set_raw::<_,_,Mut>(l, x, e, ())
}

pub fn set_own<I, O>(l: impl Lens<I, O>, x: I, e: O) -> I{
    set_raw::<_,_,Own>(l, x, e, ())
}

pub fn set_copy<I: Clone, O>(l: impl Lens<I, O>, x: &I, e: O) -> I{
    set_raw::<_,_,Cloned>(l, x, e, I::clone)
}

// pin.........