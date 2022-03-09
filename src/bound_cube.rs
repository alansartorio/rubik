use cgmath::Vector3;
use glium::{backend::Facade, Frame};

use crate::{
    cube::{Cube, CubeRotation, FaceId::*, MiddleRotation, Movement, Step},
    graphic_cube::GraphicCube,
};

pub struct BoundCube {
    cube: Cube,
    graphic_cube: GraphicCube,
}

mod layers {

    #[rustfmt::skip]
    pub const X: [u8; 3 * 3 * 6] = [
        2, 1, 0, 
        2, 1, 0, 
        2, 1, 0,

        2, 1, 0,
        2, 1, 0,
        2, 1, 0,

        0, 0, 0,
        0, 0, 0,
        0, 0, 0,

        2, 2, 2,
        2, 2, 2,
        2, 2, 2,

        2, 1, 0,
        2, 1, 0,
        2, 1, 0,

        0, 1, 2,
        0, 1, 2,
        0, 1, 2,
    ];

    #[rustfmt::skip]
    pub const Y: [u8; 3 * 3 * 6] = [
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,

        2, 2, 2,
        2, 2, 2,
        2, 2, 2,

        0, 0, 0,
        1, 1, 1,
        2, 2, 2,

        0, 0, 0,
        1, 1, 1,
        2, 2, 2,

        0, 0, 0,
        1, 1, 1,
        2, 2, 2,

        0, 0, 0,
        1, 1, 1,
        2, 2, 2,
    ];

    #[rustfmt::skip]
    pub const Z: [u8; 3 * 3 * 6] = [
        2, 2, 2,
        1, 1, 1,
        0, 0, 0,

        0, 0, 0,
        1, 1, 1,
        2, 2, 2,
        
        0, 1, 2,
        0, 1, 2,
        0, 1, 2,

        2, 1, 0,
        2, 1, 0,
        2, 1, 0,

        0, 0, 0,
        0, 0, 0,
        0, 0, 0,

        2, 2, 2,
        2, 2, 2,
        2, 2, 2,
    ];
}

impl BoundCube {
    pub fn new<F: Facade>(facade: &F) -> BoundCube {
        BoundCube {
            graphic_cube: GraphicCube::new(facade),
            cube: Cube::solved(),
        }
    }

    pub fn is_solved(&self) -> bool {
        self.cube.is_solved()
    }

    fn update_colors(&mut self) {
        self.graphic_cube.update_colors(&self.cube);
    }

    pub fn scramble(&mut self) {
        self.cube.scramble();
        self.update_colors();
    }

    pub fn solve(&mut self) {
        self.cube.solve();
        self.update_colors();
    }

    pub fn tick(&mut self, dt: f32) {
        self.graphic_cube.tick(dt);
    }

    pub fn draw(&self, target: &mut Frame) {
        self.graphic_cube.draw(target);
    }

    pub fn apply_step(&mut self, step: Step) {
        let affected_layers = match step.movement {
            Movement::CubeRotation(_) => vec![0, 1, 2],
            Movement::Rotation(face) => vec![match face {
                Right | Up | Front => 0,
                Left | Down | Back => 2,
            }],
            Movement::DoubleRotation(face) => vec![
                match face {
                    Right | Up | Front => 0,
                    Left | Down | Back => 2,
                },
                1,
            ],
            Movement::MiddleRotation(_) => vec![1],
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

        self.cube.apply_step(step);

        self.graphic_cube.update_colors(&self.cube);
        self.graphic_cube.update_rotations(
            |t| affected_layers.contains(&layer_direction[t as usize]),
            -rotation,
        );
    }
}
