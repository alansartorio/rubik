use glium::backend::Facade;

use crate::bound_cube::{BoundCube, BoundCubeTrait};

#[rustfmt::skip]
pub fn create_cube<F: Facade>(facade: &F, cube_size: usize) -> Box<dyn BoundCubeTrait> {
    macro_rules! impl_cube {
        ($( $size:expr ),*) => {
            match cube_size {
                $($size => Box::new(BoundCube::<$size>::new(facade)),)+
                _ => panic!()
            }
        };
    }

    impl_cube!(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 
        10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
        30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
        40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
        50, 51, 52, 53, 54, 55, 56, 57, 58, 59
    )
}

