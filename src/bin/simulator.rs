#![windows_subsystem = "windows"]

use std::{borrow::Borrow, env};

use glium::glutin::config::ConfigTemplateBuilder;
use glium::glutin::surface::WindowSurface;
use glium::Display;
use glium_glyph::{
    glyph_brush::{
        ab_glyph::{FontRef, PxScale},
        BuiltInLineBreaker, HorizontalAlign, Layout, Section, Text, VerticalAlign,
    },
    GlyphBrushBuilder,
};
use rubik::step::Movement;
use rubik::{bound_cube::BoundCubeTrait, step::NotationStep};

use stopwatch::Stopwatch;

use glium::backend::glutin::SimpleWindowBuilder;
use glium::winit::event::{ElementState, WindowEvent};
use glium::winit::event_loop::EventLoop;
use glium::winit::keyboard::{KeyCode, PhysicalKey};
#[allow(unused_imports)]
use glium::Surface;
use rubik::helper;

use rubik::get_cube::create_cube;
use rubik::helper::Action;

fn main() {
    let event_loop = EventLoop::new().unwrap();
    let (window, display) = SimpleWindowBuilder::new()
        .with_config_template_builder(
            ConfigTemplateBuilder::new()
                .with_multisampling(4)
                .with_depth_size(24),
        )
        .build(&event_loop);

    let font_data = include_bytes!("../../fonts/Gidole-Regular.ttf");
    let font = FontRef::try_from_slice(font_data).unwrap();
    let mut glyph_brush = GlyphBrushBuilder::using_font(font).build(&display);

    let mut cube_size = 3;
    let mut cube = create_cube(&display, cube_size);
    cube.scramble();

    let mut timer = Stopwatch::new();
    let mut timer_enabled = true;

    let mut delta_times: Vec<f32> = vec![];

    let mut draw = move |cube: &dyn BoundCubeTrait,
                         timer: &Stopwatch,
                         display: &Display<WindowSurface>,
                         dt: f32| {
        let (width, height) = display.get_framebuffer_dimensions();

        glyph_brush.queue(
            Section::default()
                .add_text(
                    Text::new(&format!("{:0.2}", timer.elapsed().as_secs_f32()))
                        .with_color([1., 1., 1., 1.])
                        .with_scale(PxScale::from(60.)),
                )
                .with_bounds((width as f32, height as f32 / 2.0))
                .with_screen_position((50., 50.)),
        );

        if env::var("SHOW_FPS").map(|var| var == "1").unwrap_or(false) {
            delta_times.push(dt);
            if delta_times.len() > 10 {
                delta_times.remove(0);
            }
            let mean_delta = delta_times.iter().sum::<f32>() / delta_times.len() as f32;

            glyph_brush.queue(
                Section::default()
                    .add_text(
                        Text::new(&format!("{:0.2}", 1. / mean_delta))
                            .with_color([1., 1., 1., 1.])
                            .with_scale(PxScale::from(60.)),
                    )
                    .with_bounds((200., height as f32 / 2.0))
                    .with_screen_position((width as f32 - 50., 50.))
                    .with_layout(Layout::SingleLine {
                        line_breaker: BuiltInLineBreaker::default(),
                        h_align: HorizontalAlign::Right,
                        v_align: VerticalAlign::Top,
                    }),
            );
        }

        let mut target = display.draw();

        target.clear_color_and_depth((0.0, 0.0, 0.0, 1.0), 1.0);
        glyph_brush.draw_queued(display, &mut target);

        cube.draw(&mut target);
        target.finish().unwrap();
    };

    let mut frame_timer = Stopwatch::new();

    helper::run_loop(event_loop, window, move |events| {
        let dt = frame_timer.elapsed().as_secs_f32();
        frame_timer.restart();

        cube.tick(dt);

        draw(cube.borrow(), &timer, &display, dt);

        let mut action = Action::Continue;
        for event in events {
            match event {
                WindowEvent::Resized(new_size) => {
                    display.resize((*new_size).into());
                }
                WindowEvent::KeyboardInput {
                    event: key_event, ..
                } => {
                    if let ElementState::Pressed = key_event.state {
                        if let PhysicalKey::Code(keycode) = key_event.physical_key {
                            if let KeyCode::Escape = keycode {
                                action = Action::Stop;
                            } else {
                                let movement = match keycode {
                                    KeyCode::KeyI => Some("R"),
                                    KeyCode::KeyK => Some("R'"),
                                    KeyCode::KeyE => Some("L'"),
                                    KeyCode::KeyD => Some("L"),
                                    KeyCode::KeyJ => Some("U"),
                                    KeyCode::KeyF => Some("U'"),
                                    KeyCode::KeyL => Some("D'"),
                                    KeyCode::KeyS => Some("D"),
                                    KeyCode::KeyG => Some("F'"),
                                    KeyCode::KeyH => Some("F"),
                                    KeyCode::KeyW => Some("B"),
                                    KeyCode::KeyO => Some("B'"),
                                    KeyCode::KeyR => Some("l'"),
                                    KeyCode::KeyU => Some("r"),
                                    KeyCode::KeyV => Some("l"),
                                    KeyCode::KeyM => Some("r'"),
                                    KeyCode::KeyC => Some("u'"),
                                    KeyCode::Comma => Some("u"),
                                    KeyCode::KeyZ => Some("d"),
                                    KeyCode::Slash => Some("d'"),
                                    KeyCode::KeyX | KeyCode::Period => Some("M'"),
                                    KeyCode::Digit5 | KeyCode::Digit6 => Some("M"),
                                    KeyCode::KeyT | KeyCode::KeyY => Some("x"),
                                    KeyCode::KeyB | KeyCode::KeyN => Some("x'"),
                                    KeyCode::Semicolon => Some("y"),
                                    KeyCode::KeyA => Some("y'"),
                                    KeyCode::KeyP => Some("z"),
                                    KeyCode::KeyQ => Some("z'"),
                                    KeyCode::Space => {
                                        cube.scramble();
                                        timer.reset();
                                        timer_enabled = true;
                                        None
                                    }
                                    KeyCode::Backspace => {
                                        cube.solve();
                                        timer.reset();
                                        timer_enabled = false;
                                        None
                                    }
                                    KeyCode::NumpadSubtract
                                    | KeyCode::Minus
                                    | KeyCode::NumpadAdd
                                    | KeyCode::Equal => {
                                        cube_size = (cube_size as i8
                                            + match keycode {
                                                KeyCode::NumpadSubtract | KeyCode::Minus => -1i8,
                                                KeyCode::NumpadAdd | KeyCode::Equal => 1i8,
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
                                    let is_rotation =
                                        !matches!(step.movement, Movement::CubeRotation(_));

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
                _ => (),
            }
        }
        action
    });
}
