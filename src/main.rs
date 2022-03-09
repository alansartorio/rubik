#![windows_subsystem = "windows"]
#[macro_use]
extern crate glium;
extern crate glium_glyph;

mod cube;
mod graphic_cube;
use crate::cube::{CubeRotation, MiddleRotation, Movement, Step};
use crate::graphic_cube::GraphicCube;
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

use stopwatch::Stopwatch;

#[allow(unused_imports)]
use glium::{glutin, Surface};
use glium::glutin::event::VirtualKeyCode;

use crate::helper::Action;
mod helper;

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
    //let index_buffer =
    //glium::IndexBuffer::new(&display, PrimitiveType::TrianglesList, &[0u16, 1, 2]).unwrap();

    let font_data = include_bytes!("../fonts/Gidole-Regular.ttf");
    let font = Font::from_bytes(font_data).unwrap();
    let mut glyph_brush = GlyphBrush::new(&display, [font]);

    let cube = Cube::solved();
    cube.scramble();
    let mut graphic_cube = GraphicCube::new(&display);
    graphic_cube.update_colors(&cube);

    let mut timer = Stopwatch::new();
    let mut timer_enabled = true;
    let mut draw = move |graphic_cube: &GraphicCube, timer: &Stopwatch| {
        let (width, height) = display.get_framebuffer_dimensions();
        //let rotation = timer.elapsed().unwrap().as_secs_f32();
        //println!("{}", &rotation);
        //let view: [[f32; 4]; 4] = Matrix4::from_nonuniform_scale(1000.0/width as f32, 1000.0/height as f32, 1.).into();
        //let projection: [[f32; 4]; 4] = ortho(
        //-(width as f32) / 1000.0,
        //width as f32 / 1000.0,
        //-(height as f32) / 1000.0,
        //height as f32 / 1000.0,
        //0.1,
        //100.0,
        //)
        //.into();

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

        graphic_cube.draw(&mut target);
        target.finish().unwrap();
    };

    helper::run_loop(event_loop, move |events| {
        graphic_cube.tick(0.007);

        draw(&graphic_cube, &timer);

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
                                    let movement = match keycode {
                                        VirtualKeyCode::I => Some("R"),
                                        VirtualKeyCode::K => Some("R'"),
                                        VirtualKeyCode::E => Some("L'"),
                                        VirtualKeyCode::D => Some("L"),
                                        VirtualKeyCode::J => Some("U"),
                                        VirtualKeyCode::F => Some("U'"),
                                        VirtualKeyCode::L => Some("D"),
                                        VirtualKeyCode::S => Some("D'"),
                                        VirtualKeyCode::G => Some("F'"),
                                        VirtualKeyCode::H => Some("F"),
                                        VirtualKeyCode::W => Some("B"),
                                        VirtualKeyCode::O => Some("B'"),
                                        VirtualKeyCode::R => Some("l'"),
                                        VirtualKeyCode::U => Some("r"),
                                        VirtualKeyCode::V => Some("l"),
                                        VirtualKeyCode::M => Some("r'"),
                                        VirtualKeyCode::C => Some("u'"),
                                        VirtualKeyCode::Comma => Some("u"),
                                        VirtualKeyCode::Z => Some("d"),
                                        VirtualKeyCode::Slash => Some("d'"),
                                        VirtualKeyCode::X | VirtualKeyCode::Period => Some("M'"),
                                        VirtualKeyCode::Key5 | VirtualKeyCode::Key6 => Some("M"),
                                        VirtualKeyCode::T | VirtualKeyCode::Y => Some("x"),
                                        VirtualKeyCode::B | VirtualKeyCode::N => Some("x'"),
                                        VirtualKeyCode::Semicolon => Some("y"),
                                        VirtualKeyCode::A => Some("y'"),
                                        VirtualKeyCode::P => Some("z"),
                                        VirtualKeyCode::Q => Some("z'"),
                                        VirtualKeyCode::Space => {
                                            cube.scramble();
                                            timer.reset();
                                            timer_enabled = true;
                                            graphic_cube.update_colors(&cube);
                                            None
                                        }
                                        VirtualKeyCode::Back => {
                                            cube.solve();
                                            timer.reset();
                                            timer_enabled = false;
                                            graphic_cube.update_colors(&cube);
                                            None
                                        }
                                        _ => None,
                                    };

                                    if let Some(step) = movement {
                                        let step: Step = step.parse().unwrap();
                                        let is_rotation = match step.movement {
                                            Movement::CubeRotation(_) => false,
                                            _ => true,
                                        };
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
                                            Movement::Rotation(face)
                                            | Movement::DoubleRotation(face) => {
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

                                        cube.apply_step(step);

                                        graphic_cube.update_colors(&cube);
                                        graphic_cube.update_rotations(
                                            |t| {
                                                affected_layers
                                                    .contains(&layer_direction[t as usize])
                                            },
                                            -rotation,
                                        );

                                        if is_rotation && !timer.is_running() && timer_enabled {
                                            timer.restart();
                                        }
                                        if is_rotation && cube.is_solved() {
                                            timer.stop();
                                            timer_enabled = false;
                                        }
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
