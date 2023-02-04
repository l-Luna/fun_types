
// the Lens trait, plus a boxed Lens to make things simpler

trait Lens<A, B>{
    fn get(&self, a: A) -> B;
    fn with(&self, a: A, b: B) -> A;
}

type DLens<A, B> = Box<dyn Lens<A, B>>;

fn boxed<A, B>(l: impl Lens<A, B> + 'static) -> DLens<A, B>{
    return Box::new(l);
}

// a "fill in the gaps" dynamic Lens

struct BoxLens<A, B>{
    getter: Box<dyn Fn(A) -> B>,
    setter: Box<dyn Fn(A, B) -> A>
}

impl<A, B> Lens<A, B> for BoxLens<A, B>{
    fn get(&self, a: A) -> B {
        (self.getter)(a)
    }

    fn with(&self, a: A, b: B) -> A {
        (self.setter)(a, b)
    }
}

// same deal, but with function pointers over boxed closures
// expected to be used by derives for efficiency

struct PtrLens<A, B>{
    getter: fn(A) -> B,
    setter: fn(A, B) -> A
}

impl<A, B> Lens<A, B> for PtrLens<A, B>{
    fn get(&self, a: A) -> B {
        (self.getter)(a)
    }

    fn with(&self, a: A, b: B) -> A {
        (self.setter)(a, b)
    }
}

// the composition of two Lenses

struct ChainLens<A, B, C>{
    l: DLens<A, B>,
    r: DLens<B, C>
}

impl<A, B, C> ChainLens<A, B, C>{
    fn new(l: impl Lens<A, B> + 'static, r: impl Lens<B, C> + 'static) -> Self{
        ChainLens{
            l: boxed(l), r: boxed(r)
        }
    }
}

impl<A, B, C> Lens<A, C> for ChainLens<A, B, C>
    where A: Clone
{
    fn get(&self, a: A) -> C{
        self.r.get(self.l.get(a))
    }

    fn with(&self, a: A, c: C) -> A{
        self.l.with(a.clone(), self.r.with(self.l.get(a), c))
    }
}

// basic Lens notation

macro_rules! lens{
    ( $t:ident . $f:ident ) => {
        PtrLens{
            getter: (|p: $t| p.$f),
            setter: (|p: $t, v| $t { $f: v, ..p })
        }
    };
    ( $x:expr ) => { $x };
    ( ($($l:tt)+) => ($($r:tt)+) ) => { ChainLens::new(lens!($($l)*), lens!($($r)*)) };
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn it_works(){
        #[derive(Clone, Copy, Debug)]
        struct Point{
            x: i32, y: i32
        }

        #[derive(Clone, Copy, Debug)]
        struct Square{
            tl: Point, br: Point
        }

        let p_lens: PtrLens<Point, i32> = lens!(Point.x);
        let sq_lens: PtrLens<Square, Point> = lens!(Square.tl);
        let fused = lens!((sq_lens) => (p_lens));
        let alt_fused = lens!((Square.tl) => (Point.y));

        let sq: Square = Square{ tl: Point { x: 3, y: 2 }, br: Point {x: 8, y: 0} };
        let altered_x = fused.with(sq, 8);
        let altered_y = alt_fused.with(sq, 22);
        println!("it goes from {sq:?} to {altered_x:?} or maybe {altered_y:?}");
    }
}
