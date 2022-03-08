#![windows_subsystem = "windows"]
#[macro_use]
extern crate glium;
extern crate glium_glyph;

mod cube;
use crate::cube::{CubeRotation, MiddleRotation, Movement, Step};
use cgmath::*;
use cube::Cube;
use cube::FaceId::*;
use glium_glyph::{
    glyph_brush::{
        rusttype::{self, Font},
        Section,
    },
    GlyphBrush,
};

use std::{f32::consts::PI, ops::Mul};
use stopwatch::Stopwatch;

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

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new()
        .with_depth_buffer(24)
        .with_multisampling(4)
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
        rotation_from: [f32; 3],
    }
    let mut per_instance = {
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
                                    .into()),
                                color: [0., 0., 0.],
                                rotation_from: [0., 0., 0.],
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
            uniform float animation_lerp;
            
            in vec2 position;
            in vec3 color;
            in mat4 model_to_world;
            in vec3 rotation_from;

            out vec3 vColor;

            mat4 rotationX(float angle) {
                float s = sin(angle);
                float c = cos(angle);

                return mat4(
                    1., 0., 0., 0.,
                    0., c, s, 0.,
                    0., -s, c, 0.,
                    0., 0., 0., 1.
                );
            }
            mat4 rotationY(float angle) {
                float s = sin(angle);
                float c = cos(angle);

                return mat4(
                    c, 0., -s, 0.,
                    0., 1., 0., 0.,
                    s, 0., c, 0.,
                    0., 0., 0., 1.
                );
            }
            mat4 rotationZ(float angle) {
                float s = sin(angle);
                float c = cos(angle);

                return mat4(
                    c, s, 0., 0.,
                    -s, c, 0., 0.,
                    0., 0., 1., 0.,
                    0., 0., 0., 1.
                );
            }

            void main() {
                vec3 rotation = rotation_from * animation_lerp;
                gl_Position = projection * view * rotationY(rotation.y) * rotationZ(rotation.z) * rotationX(rotation.x) * model_to_world * vec4(position * 0.1, 0.0, 1.0);// * matrix;
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

    let font_data = include_bytes!("../fonts/Gidole-Regular.ttf");
    let font = Font::from_bytes(font_data).unwrap();
    let mut glyph_brush = GlyphBrush::new(&display, [font]);

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

    fn update_rotations<F: Fn(u8) -> bool>(
        marked: F,
        rot: Vector3<i8>,
        per_instance: &mut VertexBuffer<Attr>,
        animation_lerp: f32,
    ) {
        let mut mapping = per_instance.map();

        for (i, attr) in Iterator::enumerate(mapping.iter_mut()) {
            if marked(i as u8) {
                let vec: [i8; 3] = rot.into();
                attr.rotation_from = vec.map(|v| v as f32 * PI / 2.).into();
            } else {
                let current = attr.rotation_from.map(|v| v * animation_lerp).into();
                attr.rotation_from = current;
            }
        }
    }

    let cube = Cube::solved();
    cube.scramble();
    update_colors(&cube, &mut per_instance);

    let mut animation_lerp: f32 = 0.;
    let mut timer = Stopwatch::new();
    let mut timer_enabled = true;
    let mut draw = move |per_instance: &VertexBuffer<_>, timer: &Stopwatch, animation_lerp: f32| {
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
            animation_lerp: animation_lerp,
        };

        glyph_brush.queue(Section {
            text: format!("{:0.2}", timer.elapsed().as_secs_f32()).as_str(),
            bounds: (width as f32, height as f32 / 2.0),
            screen_position: (50., 50.),
            color: [1., 1., 1., 1.],
            scale: rusttype::Scale::uniform(60.),
            //scale: glyph_brush::Scale::uniform(10.),
            ..Section::default()
        });

        let mut target = display.draw();

        target.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 1.0);
        glyph_brush.draw_queued(&display, &mut target);

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
        animation_lerp = if animation_lerp > 0. {
            animation_lerp - 0.05
        } else {
            0.
        };

        draw(&per_instance, &timer, animation_lerp);

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
                                    let mut movement = false;
                                    let mut affected_layers = vec![0u8, 1, 2];
                                    let mut rotation = Vector3::new(0i8, 0, 0);
                                    let mut layer_direction = &layers::X;
                                    match keycode {
                                        VirtualKeyCode::I => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Right),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0];
                                            rotation = Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::K => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Right),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0];
                                            rotation = -Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::E => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Left),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![2];
                                            rotation = Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::D => {
                                            cube.apply_step(Step::new(Movement::Rotation(Left), 1));
                                            movement = true;
                                            affected_layers = vec![2];
                                            rotation = -Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::J => {
                                            cube.apply_step(Step::new(Movement::Rotation(Up), 1));
                                            movement = true;
                                            affected_layers = vec![0];
                                            rotation = -Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::F => {
                                            cube.apply_step(Step::new(Movement::Rotation(Up), -1));
                                            movement = true;
                                            affected_layers = vec![0];
                                            rotation = Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::L => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Down),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![2];
                                            rotation = -Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::S => {
                                            cube.apply_step(Step::new(Movement::Rotation(Down), 1));
                                            movement = true;
                                            affected_layers = vec![2];
                                            rotation = Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::G => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Front),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0];
                                            rotation = -Vector3::unit_z();
                                            layer_direction = &layers::Z;
                                        }
                                        VirtualKeyCode::H => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Front),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0];
                                            rotation = Vector3::unit_z();
                                            layer_direction = &layers::Z;
                                        }
                                        VirtualKeyCode::W => {
                                            cube.apply_step(Step::new(Movement::Rotation(Back), 1));
                                            movement = true;
                                            affected_layers = vec![2];
                                            rotation = -Vector3::unit_z();
                                            layer_direction = &layers::Z;
                                        }
                                        VirtualKeyCode::O => {
                                            cube.apply_step(Step::new(
                                                Movement::Rotation(Back),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![2];
                                            rotation = Vector3::unit_z();
                                            layer_direction = &layers::Z;
                                        }
                                        VirtualKeyCode::R => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Left),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![1, 2];
                                            rotation = Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::U => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Right),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0, 1];
                                            rotation = Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::V => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Left),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![1, 2];
                                            rotation = -Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::M => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Right),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0, 1];
                                            rotation = -Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::C => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Up),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0, 1];
                                            rotation = Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::Comma => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Up),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![0, 1];
                                            rotation = -Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::Z => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Down),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![1, 2];
                                            rotation = Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::Slash => {
                                            cube.apply_step(Step::new(
                                                Movement::DoubleRotation(Down),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![1, 2];
                                            rotation = -Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::X | VirtualKeyCode::Period => {
                                            cube.apply_step(Step::new(
                                                Movement::MiddleRotation(MiddleRotation::M),
                                                -1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![1];
                                            rotation = Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::Key5 | VirtualKeyCode::Key6 => {
                                            cube.apply_step(Step::new(
                                                Movement::MiddleRotation(MiddleRotation::M),
                                                1,
                                            ));
                                            movement = true;
                                            affected_layers = vec![1];
                                            rotation = -Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::T | VirtualKeyCode::Y => {
                                            cube.apply_step(Step::new(
                                                Movement::CubeRotation(CubeRotation::X),
                                                1,
                                            ));
                                            affected_layers = vec![0, 1, 2];
                                            rotation = Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::B | VirtualKeyCode::N => {
                                            cube.apply_step(Step::new(
                                                Movement::CubeRotation(CubeRotation::X),
                                                -1,
                                            ));
                                            affected_layers = vec![0, 1, 2];
                                            rotation = -Vector3::unit_x();
                                            layer_direction = &layers::X;
                                        }
                                        VirtualKeyCode::Semicolon => {
                                            cube.apply_step(Step::new(
                                                Movement::CubeRotation(CubeRotation::Y),
                                                1,
                                            ));
                                            affected_layers = vec![0, 1, 2];
                                            rotation = -Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::A => {
                                            cube.apply_step(Step::new(
                                                Movement::CubeRotation(CubeRotation::Y),
                                                -1,
                                            ));
                                            affected_layers = vec![0, 1, 2];
                                            rotation = Vector3::unit_y();
                                            layer_direction = &layers::Y;
                                        }
                                        VirtualKeyCode::P => {
                                            cube.apply_step(Step::new(
                                                Movement::CubeRotation(CubeRotation::Z),
                                                1,
                                            ));
                                            affected_layers = vec![0, 1, 2];
                                            rotation = Vector3::unit_z();
                                            layer_direction = &layers::Z;
                                        }
                                        VirtualKeyCode::Q => {
                                            cube.apply_step(Step::new(
                                                Movement::CubeRotation(CubeRotation::Z),
                                                -1,
                                            ));
                                            affected_layers = vec![0, 1, 2];
                                            rotation = -Vector3::unit_z();
                                            layer_direction = &layers::Z;
                                        }
                                        VirtualKeyCode::Space => {
                                            cube.scramble();
                                            timer.reset();
                                            timer_enabled = true;
                                        }
                                        VirtualKeyCode::Back => {
                                            cube.solve();
                                            timer.reset();
                                        }
                                        _ => (),
                                    };
                                    update_colors(&cube, &mut per_instance);
                                    update_rotations(
                                        |t| affected_layers.contains(&layer_direction[t as usize]),
                                        -rotation,
                                        &mut per_instance,
                                        animation_lerp,
                                    );
                                    animation_lerp = 1.;
                                    if movement && !timer.is_running() && timer_enabled {
                                        timer.restart();
                                    }
                                    if cube.is_solved() {
                                        timer.stop();
                                        timer_enabled = false;
                                    }
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
