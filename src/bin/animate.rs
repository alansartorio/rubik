use std::{
    io::{self, BufRead},
    sync::mpsc::channel,
    thread,
    time::Duration, env,
};

use glium::{glutin, Surface, backend::Facade};
use rubik::{
    bound_cube::{BoundCube, BoundCubeTrait},
    helper,
    step::NotationStep,
};
use stopwatch::Stopwatch;

#[rustfmt::skip]
pub fn parse_cube<F: Facade>(facade: &F, cube_size: usize, cube_string: String) -> Box<dyn BoundCubeTrait> {
    macro_rules! impl_cube {
        ($( $size:expr ),*) => {
            match cube_size {
                $($size => Box::new({
                    let cube = cube_string.parse().unwrap();
                    BoundCube::<$size>::from_cube(facade, cube)
                }),)+
                _ => panic!()
            }
        };
    }

    impl_cube!(
        1, 2, 3, 4, 5, 6, 7, 8, 9, 
        10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
        30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
        40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
        50, 51, 52, 53, 54, 55, 56, 57, 58, 59
    )
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let cube_size = {
        if args.len() == 2 {
            args[1].parse().expect("First argument must be cube size (integer).")
        } else {
            3
        }
    };
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

    let mut cube = parse_cube(&display, cube_size, cube_string);

    let (tx, rx) = channel::<NotationStep>();

    thread::spawn(move || {
        let stdin = io::stdin();
        let lines = stdin.lock().lines().map(|line| line.unwrap());

        let steps = lines.map(|line| line.parse::<NotationStep>().unwrap());

        thread::sleep(Duration::from_millis(1000));
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
            cube.apply_notation_step(step);
        }
        cube.tick(dt);

        let mut target = display.draw();

        target.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 1.0);
        cube.draw(&mut target);
        target.finish().unwrap();

        helper::Action::Continue
    })
}
