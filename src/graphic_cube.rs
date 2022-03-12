use std::{f32::consts::PI, ops::Mul};

use cgmath::{perspective, Matrix4, Point3, Rad, SquareMatrix, Vector3};
use glium::{
    backend::Facade,
    implement_vertex,
    index::{NoIndices, PrimitiveType},
    uniform, Frame, Program, Surface, VertexBuffer,
};

use crate::cube::{self, Cube};

#[derive(Copy, Clone, Debug)]
struct Attr {
    model_to_world: [[f32; 4]; 4],
    color: [f32; 3],
    rotation_from: [f32; 3],
}

#[derive(Copy, Clone)]
struct Vertex {
    position: [f32; 2],
}

pub struct GraphicCube<const N: usize> {
    view: [[f32; 4]; 4],
    program: Program,
    per_instance: VertexBuffer<Attr>,
    indices: NoIndices,
    vertex_buffer: VertexBuffer<Vertex>,
    animation_lerp: f32,
}

mod colors {
    type Color = [f32; 3];
    pub static RED: Color = [1.0, 0.0, 0.0];
    pub static GREEN: Color = [0.0, 1.0, 0.0];
    pub static BLUE: Color = [0.0, 0.0, 1.0];
    pub static YELLOW: Color = [1.0, 1.0, 0.0];
    pub static WHITE: Color = [1.0, 1.0, 1.0];
    pub static ORANGE: Color = [1.0, 0.3, 0.0];
    pub static BLACK: Color = [0.0, 0.0, 0.0];
}

impl<const N: usize> GraphicCube<N> {
    pub fn new<F: Facade>(facade: &F) -> GraphicCube<N> {
        GraphicCube {
            view: Matrix4::look_at_rh(
                Point3::new(0., 0.25 * (N as f32), -0.25 * (N as f32)),
                Point3::new(0., 0., 0.),
                Vector3::new(0., 1., 0.),
            )
            .into(),
            animation_lerp: 0.,
            program: glium::Program::from_source(
                facade,
                include_str!("shader/vert.glsl"),
                include_str!("shader/frag.glsl"),
                None,
            )
            .unwrap(),
            indices: glium::index::NoIndices(PrimitiveType::TriangleFan),
            vertex_buffer: {
                implement_vertex!(Vertex, position);

                glium::VertexBuffer::new(
                    facade,
                    &[
                        Vertex {
                            position: [-0.5, -0.5],
                        },
                        Vertex {
                            position: [-0.5, 0.5],
                        },
                        Vertex {
                            position: [0.5, 0.5],
                        },
                        Vertex {
                            position: [0.5, -0.5],
                        },
                    ],
                )
                .unwrap()
            },
            per_instance: {
                implement_vertex!(Attr, model_to_world, color, rotation_from);

                //let colors = [
                //[colors::RED, colors::ORANGE, colors::WHITE],
                //[colors::GREEN, colors::YELLOW, colors::BLUE],
                //[colors::RED, colors::ORANGE, colors::WHITE],
                //];
                const HALF_PI: f32 = PI / 2.;
                let rotations = [
                    Matrix4::from_angle_x(Rad(HALF_PI)),
                    Matrix4::from_angle_x(Rad(-HALF_PI)),
                    Matrix4::from_angle_y(Rad(HALF_PI)),
                    Matrix4::from_angle_y(Rad(-HALF_PI)),
                    Matrix4::identity(),
                    Matrix4::from_angle_y(Rad(PI)),
                ];
                let data = rotations
                    .iter()
                    .flat_map(|rotation| {
                        (0..N)
                            .flat_map(|y| {
                                (0..N)
                                    .map(|x| Attr {
                                        model_to_world: (rotation
                                            .mul(Matrix4::from_translation(Vector3::new(
                                                -((x as f32) - (N as f32) / 2. + 0.5) * 0.15,
                                                -((y as f32) - (N as f32) / 2. + 0.5) * 0.15,
                                                -0.15 * (N as f32) / 2.,
                                            )))
                                            .into()),
                                        color: [0., 0., 0.],
                                        rotation_from: [0., 0., 0.],
                                    })
                                    .collect::<Vec<_>>()
                            })
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>();
                //println!("{:?}", data.first());
                glium::VertexBuffer::dynamic(facade, &data).unwrap()
            },
        }
    }

    pub fn draw(&self, target: &mut Frame) {
        let (width, height) = target.get_dimensions();

        let projection: [[f32; 4]; 4] =
            perspective(Rad(PI / 4.), width as f32 / height as f32, 0.1, 100.).into();

        let uniforms = uniform! {
            view: self.view,
            projection: projection,
            animation_lerp: self.animation_lerp,
        };

        target
            .draw(
                (
                    &self.vertex_buffer,
                    self.per_instance.per_instance().unwrap(),
                ),
                &self.indices,
                &self.program,
                &uniforms,
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLess,
                        write: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            )
            .unwrap();
    }

    pub fn update_colors(&mut self, cube: &Cube<N>) {
        let mut mapping = self.per_instance.map();
        for (attr, color) in Iterator::zip(mapping.iter_mut(), cube.flatten_stickers()) {
            attr.color = match color.0 {
                Some(face) => match face {
                    cube::FaceId::Up => colors::RED,
                    cube::FaceId::Down => colors::ORANGE,
                    cube::FaceId::Right => colors::WHITE,
                    cube::FaceId::Left => colors::YELLOW,
                    cube::FaceId::Front => colors::BLUE,
                    cube::FaceId::Back => colors::GREEN,
                },
                None => colors::BLACK,
            };
        }
    }

    pub fn update_rotations<F: Fn(usize) -> bool>(&mut self, marked: F, rot: Vector3<i8>) {
        let mut mapping = self.per_instance.map();

        for (i, attr) in Iterator::enumerate(mapping.iter_mut()) {
            if marked(i) {
                let vec: [i8; 3] = rot.into();
                attr.rotation_from = vec.map(|v| v as f32 * PI / 2.).into();
            } else {
                let animation_lerp = self.animation_lerp;
                let current = attr.rotation_from.map(move |v| v * animation_lerp).into();
                attr.rotation_from = current;
            }
        }

        self.animation_lerp = 1.;
    }

    pub fn tick(&mut self, dt: f32) {
        self.animation_lerp -= dt * 7.;
        if self.animation_lerp < 0. {
            self.animation_lerp = 0.;
        }
    }
}
