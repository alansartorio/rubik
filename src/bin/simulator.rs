#![windows_subsystem = "windows"]

use std::{borrow::Borrow, env};

use glium::Display;
use glium_glyph::{
    glyph_brush::{
        rusttype::{self, Font},
        BuiltInLineBreaker, HorizontalAlign, Layout, Section, VerticalAlign,
    },
    GlyphBrush,
};
use rubik::step::Movement;
use rubik::{bound_cube::BoundCubeTrait, step::NotationStep};

use stopwatch::Stopwatch;

use glium::glutin::event::VirtualKeyCode;
#[allow(unused_imports)]
use glium::{glutin, Surface};
use rubik::helper;

use rubik::get_cube::create_cube;
use rubik::helper::Action;

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new()
        .with_depth_buffer(24)
        .with_multisampling(4)
        .with_vsync(true);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();

    let font_data = include_bytes!("../../fonts/Gidole-Regular.ttf");
    let font = Font::from_bytes(font_data).unwrap();
    let mut glyph_brush = GlyphBrush::new(&display, [font]);

    let mut cube_size = 3;
    let mut cube = create_cube(&display, cube_size);
    cube.scramble();

    let mut timer = Stopwatch::new();
    let mut timer_enabled = true;

    let mut delta_times: Vec<f32> = vec![];

    let mut draw =
        move |cube: &dyn BoundCubeTrait, timer: &Stopwatch, display: &Display, dt: f32| {
            let (width, height) = display.get_framebuffer_dimensions();

            glyph_brush.queue(Section {
                text: format!("{:0.2}", timer.elapsed().as_secs_f32()).as_str(),
                bounds: (width as f32, height as f32 / 2.0),
                screen_position: (50., 50.),
                color: [1., 1., 1., 1.],
                scale: rusttype::Scale::uniform(60.),
                ..Section::default()
            });

            if env::var("SHOW_FPS").map(|var| var == "1").unwrap_or(false) {
                delta_times.push(dt);
                if delta_times.len() > 10 {
                    delta_times.remove(0);
                }
                let mean_delta = delta_times.iter().sum::<f32>() / delta_times.len() as f32;

                glyph_brush.queue(Section {
                    text: format!("{:0.2}", 1. / mean_delta).as_str(),
                    bounds: (200., height as f32 / 2.0),
                    screen_position: (width as f32 - 50., 50.),
                    color: [1., 1., 1., 1.],
                    scale: rusttype::Scale::uniform(60.),
                    layout: Layout::SingleLine {
                        line_breaker: BuiltInLineBreaker::default(),
                        h_align: HorizontalAlign::Right,
                        v_align: VerticalAlign::Top,
                    },
                    ..Section::default()
                });
            }

            let mut target = display.draw();

            target.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 1.0);
            glyph_brush.draw_queued(display, &mut target);

            cube.draw(&mut target);
            target.finish().unwrap();
        };

    let mut frame_timer = Stopwatch::new();

    helper::run_loop(event_loop, move |events| {
        let dt = frame_timer.elapsed().as_secs_f32();
        frame_timer.restart();

        cube.tick(dt);

        draw(cube.borrow(), &timer, &display, dt);

        let mut action = Action::Continue;
        for event in events {
            if let glutin::event::Event::WindowEvent {
                event: glutin::event::WindowEvent::KeyboardInput { input, .. },
                ..
            } = event
            {
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
                                VirtualKeyCode::L => Some("D'"),
                                VirtualKeyCode::S => Some("D"),
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
                                    None
                                }
                                VirtualKeyCode::Back => {
                                    cube.solve();
                                    timer.reset();
                                    timer_enabled = false;
                                    None
                                }
                                VirtualKeyCode::NumpadSubtract
                                | VirtualKeyCode::Minus
                                | VirtualKeyCode::NumpadAdd
                                | VirtualKeyCode::Equals
                                | VirtualKeyCode::Plus => {
                                    cube_size = (cube_size as i8
                                        + match keycode {
                                            VirtualKeyCode::NumpadSubtract
                                            | VirtualKeyCode::Minus => -1i8,
                                            VirtualKeyCode::NumpadAdd
                                            | VirtualKeyCode::Plus
                                            | VirtualKeyCode::Equals => 1i8,
                                            _ => unreachable!(),
                                        }) as usize;
                                    cube_size = cube_size.clamp(1, 59);
                                    cube = create_cube(&display, cube_size);
                                    timer.reset();
                                    timer_enabled = false;
                                    None
                                }
                                _ => None,
                            };

                            if let Some(step) = movement {
                                let step: NotationStep = step.parse().unwrap();
                                let is_rotation = !matches!(step.movement, Movement::CubeRotation(_));

                                cube.apply_notation_step(step);
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
        }
        action
    });
}
