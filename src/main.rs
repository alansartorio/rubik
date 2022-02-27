#![feature(maybe_uninit_uninit_array)]
#![feature(type_ascription)]
#![feature(derive_default_enum)]
#[macro_use]
extern crate glium;

mod cube;
use cgmath::{Transform, *};
use cube::Cube;

use std::{f32::consts::PI, ops::Mul, time::SystemTime};

#[allow(unused_imports)]
use glium::{glutin, Surface};
use glium::{glutin::event::VirtualKeyCode, index::PrimitiveType, VertexBuffer};

use crate::helper::Action;
mod helper;
mod colors {
    type Color = [f32; 3];
    pub static RED: Color = [1.0, 0.0, 0.0];
    pub static GREEN: Color = [0.0, 1.0, 0.0];
    pub static BLUE: Color = [0.0, 0.0, 1.0];
    pub static YELLOW: Color = [1.0, 1.0, 0.0];
    pub static WHITE: Color = [1.0, 1.0, 1.0];
    pub static ORANGE: Color = [1.0, 0.3, 0.0];
}

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new()
        .with_depth_buffer(24)
        .with_vsync(true);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();
    let vertex_buffer = {
        #[derive(Copy, Clone)]
        struct Vertex {
            position: [f32; 2],
        }

        implement_vertex!(Vertex, position);

        glium::VertexBuffer::new(
            &display,
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
    };
    #[derive(Copy, Clone, Debug)]
    struct Attr {
        model_to_world: [[f32; 4]; 4],
        color: [f32; 3],
    }
    let mut per_instance = {
        implement_vertex!(Attr, model_to_world, color);

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
            .map(|rotation| {
                (-1i16..=1)
                    .map(|y| {
                        (-1i16..=1)
                            .map(|x| Attr {
                                model_to_world: (rotation
                                    .mul(Matrix4::from_translation(Vector3::new(
                                        -x as f32 * 0.15,
                                        -y as f32 * 0.15,
                                        -0.15 * 3. / 2.,
                                    )))
                                    .into():
                                    [[f32; 4]; 4]),
                                color: [0., 0., 0.],
                            })
                            .collect::<Vec<_>>()
                    })
                    .flatten()
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect::<Vec<_>>();
        //println!("{:?}", data.first());
        glium::VertexBuffer::dynamic(&display, &data).unwrap()
    };
    //let index_buffer =
    //glium::IndexBuffer::new(&display, PrimitiveType::TrianglesList, &[0u16, 1, 2]).unwrap();
    let indices = glium::index::NoIndices(PrimitiveType::TriangleFan);

    let program = glium::Program::from_source(
        &display,
        "
            #version 140

            uniform mat4 projection;
            uniform mat4 view;
            
            in vec2 position;
            in vec3 color;
            in mat4 model_to_world;

            out vec3 vColor;

            void main() {
                gl_Position = projection * view * model_to_world * vec4(position * 0.1, 0.0, 1.0);// * matrix;
                vColor = color;
            }
        ",
        "
            #version 140

            in vec3 vColor;
            out vec4 f_color;

            void main() {
                f_color = vec4(vColor, 1.0);
            }
        ",
        None,
    )
    .unwrap();

    let update_colors = |cube: &Cube, per_instance: &mut VertexBuffer<Attr>| {
        let mut mapping = per_instance.map();
        for (attr, color) in Iterator::zip(mapping.iter_mut(), cube.flatten_stickers()) {
            //attr.color = [1. - attr.color[0], 1. - attr.color[1], 1. - attr.color[2]];
            attr.color = match color {
                cube::FaceId::Up => colors::RED,
                cube::FaceId::Down => colors::ORANGE,
                cube::FaceId::Right => colors::WHITE,
                cube::FaceId::Left => colors::YELLOW,
                cube::FaceId::Front => colors::BLUE,
                cube::FaceId::Back => colors::GREEN,
            };
        }
    };

    let cube = Cube::solved();
    cube.scramble();
    update_colors(&cube, &mut per_instance);

    //let timer = SystemTime::now();
    let draw = move |per_instance: &VertexBuffer<_>| {
        let (width, height) = display.get_framebuffer_dimensions();
        //let rotation = timer.elapsed().unwrap().as_secs_f32();
        //println!("{}", &rotation);
        //let view: [[f32; 4]; 4] = Matrix4::from_nonuniform_scale(1000.0/width as f32, 1000.0/height as f32, 1.).into();
        let view: [[f32; 4]; 4] = Matrix4::look_at_rh(
            Point3::new(0., 1., -1.),
            Point3::new(0., 0., 0.),
            Vector3::new(0., 1., 0.),
        )
        .into();
        //let projection: [[f32; 4]; 4] = ortho(
        //-(width as f32) / 1000.0,
        //width as f32 / 1000.0,
        //-(height as f32) / 1000.0,
        //height as f32 / 1000.0,
        //0.1,
        //100.0,
        //)
        //.into();
        let projection: [[f32; 4]; 4] =
            perspective(Rad(PI / 4.), width as f32 / height as f32, 0.1, 100.).into();
        let uniforms = uniform! {
            view: view,
            projection: projection,
        };

        let mut target = display.draw();

        target.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 1.0);
        target
            .draw(
                (&vertex_buffer, per_instance.per_instance().unwrap()),
                &indices,
                &program,
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
        target.finish().unwrap();
    };

    helper::run_loop(event_loop, move |events| {
        draw(&per_instance);

        let mut action = Action::Continue;
        for event in events {
            match event {
                glutin::event::Event::WindowEvent { event, .. } => match event {
                    glutin::event::WindowEvent::KeyboardInput { input, .. } => {
                        if let glutin::event::ElementState::Pressed = input.state {
                            if let Some(keycode) = input.virtual_keycode {
                                if let VirtualKeyCode::Escape = keycode {
                                    action = Action::Stop;
                                } else {
                                    match keycode {
                                        VirtualKeyCode::I => {
                                            cube.rotate_face(cube::FaceId::Right, 1)
                                        }
                                        VirtualKeyCode::K => {
                                            cube.rotate_face(cube::FaceId::Right, -1)
                                        }
                                        VirtualKeyCode::E => {
                                            cube.rotate_face(cube::FaceId::Left, -1)
                                        }
                                        VirtualKeyCode::D => {
                                            cube.rotate_face(cube::FaceId::Left, 1)
                                        }
                                        VirtualKeyCode::J => cube.rotate_face(cube::FaceId::Up, 1),
                                        VirtualKeyCode::F => cube.rotate_face(cube::FaceId::Up, -1),
                                        VirtualKeyCode::L => cube.rotate_face(cube::FaceId::Down, -1),
                                        VirtualKeyCode::S => cube.rotate_face(cube::FaceId::Down, 1),
                                        VirtualKeyCode::G => {
                                            cube.rotate_face(cube::FaceId::Front, -1)
                                        }
                                        VirtualKeyCode::H => {
                                            cube.rotate_face(cube::FaceId::Front, 1)
                                        }
                                        VirtualKeyCode::Semicolon => {
                                            cube.rotate_cube(cube::Rotation::YP)
                                        }
                                        VirtualKeyCode::A => cube.rotate_cube(cube::Rotation::YN),
                                        VirtualKeyCode::T | VirtualKeyCode::Y => cube.rotate_cube(cube::Rotation::XN),
                                        VirtualKeyCode::B | VirtualKeyCode::N => cube.rotate_cube(cube::Rotation::XP),

                                        _ => (),
                                    };
                                    update_colors(&cube, &mut per_instance);
                                }
                            }
                        }
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        return action;
    });
}
