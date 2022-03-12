use std::{
    io::{self, BufRead},
    sync::mpsc::channel,
    thread,
    time::Duration,
};

use glium::{glutin, Surface};
use rubik::{
    bound_cube::{BoundCube, BoundCubeTrait},
    cube::{Cube, Step},
    helper,
};
use stopwatch::Stopwatch;

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new()
        .with_depth_buffer(24)
        .with_multisampling(4)
        .with_vsync(true);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();

    let stdin = io::stdin();
    let lines = stdin.lock().lines().map(|line| line.unwrap());
    let cube_string = lines
        .take_while(|line| line != "===")
        .collect::<Vec<_>>()
        .join("\n");

    let cube: Cube<3> = cube_string.parse().unwrap();

    let mut cube = BoundCube::from_cube(&display, cube);

    let (tx, rx) = channel::<Step>();

    thread::spawn(move || {
        let stdin = io::stdin();
        let lines = stdin.lock().lines().map(|line| line.unwrap());

        let steps = lines.map(|line| line.parse::<Step>().unwrap());

        for step in steps {
            tx.send(step).unwrap();
            thread::sleep(Duration::from_millis(200));
        }
    });

    let mut frame_timer = Stopwatch::new();

    helper::run_loop(event_loop, move |_| {
        let dt = frame_timer.elapsed().as_secs_f32();
        frame_timer.restart();

        if let Ok(step) = rx.try_recv() {
            cube.apply_step(step);
        }
        cube.tick(dt);

        let mut target = display.draw();

        target.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 1.0);
        cube.draw(&mut target);
        target.finish().unwrap();

        helper::Action::Continue
    })
}
