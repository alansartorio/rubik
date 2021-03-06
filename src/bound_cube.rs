use cgmath::Vector3;
use glium::{backend::Facade, Frame};

use crate::{
    cube::Cube,
    graphic_cube::GraphicCube,
    step::{CubeRotation, FaceId::*, MiddleRotation, Movement, NotationAlgorythm, NotationStep},
};

pub struct BoundCube<const N: usize> {
    cube: Cube<N>,
    graphic_cube: GraphicCube<N>,
}

pub enum FaceLayers {
    ZERO,
    MAX,
    TOP,
    BOTTOM,
    RIGHT,
    LEFT,
}

mod layers {
    use super::FaceLayers::{self, *};

    pub const X: [FaceLayers; 6] = [LEFT, LEFT, ZERO, MAX, LEFT, RIGHT];

    pub const Y: [FaceLayers; 6] = [ZERO, MAX, BOTTOM, BOTTOM, BOTTOM, BOTTOM];

    pub const Z: [FaceLayers; 6] = [TOP, BOTTOM, RIGHT, LEFT, ZERO, MAX];
}

pub trait BoundCubeTrait {
    fn is_solved(&self) -> bool;

    fn scramble(&mut self);

    fn solve(&mut self);

    fn tick(&mut self, dt: f32);

    fn draw(&self, target: &mut Frame);

    fn apply_notation_step(&mut self, step: NotationStep);

    fn apply_notation_algorythm_unanimated(&mut self, algorythm: &NotationAlgorythm);
}

impl<const N: usize> BoundCube<N> {
    pub fn new<F: Facade>(facade: &F) -> BoundCube<N> {
        Self::from_cube(facade, Cube::solved())
    }

    pub fn from_cube<F: Facade>(facade: &F, cube: Cube<N>) -> BoundCube<N> {
        let mut new = BoundCube {
            graphic_cube: GraphicCube::new(facade),
            cube,
        };
        new.update_colors();
        new
    }

    fn update_colors(&mut self) {
        self.graphic_cube.update_colors(&self.cube);
    }
}

impl<const N: usize> BoundCubeTrait for BoundCube<N> {
    fn is_solved(&self) -> bool {
        self.cube.is_solved()
    }

    fn scramble(&mut self) {
        self.cube.scramble();
        self.update_colors();
    }

    fn solve(&mut self) {
        self.cube.solve();
        self.update_colors();
    }

    fn tick(&mut self, dt: f32) {
        self.graphic_cube.tick(dt);
    }

    fn draw(&self, target: &mut Frame) {
        self.graphic_cube.draw(target);
    }

    fn apply_notation_step(&mut self, step: NotationStep) {
        let start = {
            let mut a = [false; N];
            a[0] = true;
            a
        };
        let middle = {
            let mut a = [false; N];
            a[N / 2] = true;
            a
        };
        let end = {
            let mut a = [false; N];
            a[N - 1] = true;
            a
        };
        let two_start = {
            if N == 1 {
                [true; N]
            } else {
                let mut a = [false; N];
                a[0] = true;
                a[1] = true;
                a
            }
        };
        let two_end = {
            if N == 1 {
                [true; N]
            } else {
                let mut a = [false; N];
                a[N - 1] = true;
                a[N - 2] = true;
                a
            }
        };
        let all = [true; N];
        let affected_layers = match step.movement {
            Movement::CubeRotation(_) => all,
            Movement::Rotation(face) => match face {
                Right | Up | Front => start,
                Left | Down | Back => end,
            },
            Movement::DoubleRotation(face) => match face {
                Right | Up | Front => two_start,
                Left | Down | Back => two_end,
            },
            Movement::MiddleRotation(_) => middle,
        };
        let x: (Vector3<i8>, _) = (Vector3::unit_x(), &layers::X);
        let y: (Vector3<i8>, _) = (-Vector3::unit_y(), &layers::Y);
        let z: (Vector3<i8>, _) = (Vector3::unit_z(), &layers::Z);
        let (mut rotation, layer_direction) = match step.movement {
            Movement::CubeRotation(rot) => match rot {
                CubeRotation::X => x,
                CubeRotation::Y => y,
                CubeRotation::Z => z,
            },
            Movement::Rotation(face) | Movement::DoubleRotation(face) => {
                let (mut rotation, layer) = match face {
                    Right | Left => x,
                    Up | Down => y,
                    Front | Back => z,
                };
                rotation *= match face {
                    Right | Up | Front => 1,
                    _ => -1,
                };
                (rotation, layer)
            }
            Movement::MiddleRotation(rot) => {
                let (rotation, layer) = match rot {
                    MiddleRotation::M => x,
                    MiddleRotation::E => y,
                    MiddleRotation::S => z,
                };
                (-rotation, layer)
            }
        };
        rotation *= step.count;

        self.cube.apply_notation_step(step);

        self.graphic_cube.update_colors(&self.cube);
        self.graphic_cube.update_rotations(
            |t| {
                affected_layers[{
                    let f = t / (N * N);
                    let fi = t % (N * N);
                    let y = fi / N;
                    let x = fi % N;
                    match layer_direction[f] {
                        FaceLayers::MAX => N - 1,
                        FaceLayers::ZERO => 0,
                        FaceLayers::RIGHT => x,
                        FaceLayers::LEFT => N - 1 - x,
                        FaceLayers::BOTTOM => y,
                        FaceLayers::TOP => N - 1 - y,
                    }
                }]
            },
            -rotation,
        );
    }

    fn apply_notation_algorythm_unanimated(&mut self, algorythm: &NotationAlgorythm) {
        self.cube.apply_notation_algorythm(algorythm);
        self.update_colors();
    }
}
