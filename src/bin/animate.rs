use std::{ops::Neg, sync::mpsc::channel, thread, time::Duration};

use glium::{glutin, Surface};
use rubik::{
    bound_cube::BoundCube,
    cube::{Algorythm, Step},
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
    let mut cube = BoundCube::new(&display);
    let algorythm: Algorythm = "B2 D' B L' F2 U D' L' D R2 L' F2 B2 L2 D F' L2 U' B2 F D2 R D L2 U".parse().unwrap();

    for &step in &algorythm.0 {
        cube.apply_step(step);
    }

    let (tx, rx) = channel::<Step>();

    thread::spawn(move || {
        //let algorythm: Algorythm = "R U R' U'".parse().unwrap();
        let reverse = algorythm.0.iter().rev().cloned().map(Neg::neg);

        //for step in std::iter::repeat(
            //algorythm
                //.0
                //.iter()
                //.cloned()
                //.chain(algorythm.0.iter().rev().cloned().map(Neg::neg)),
        //)
        //.flatten()
        for step in reverse {
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
