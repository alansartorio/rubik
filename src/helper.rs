use crate::glutin;
use crate::glutin::event_loop::EventLoop;
use glium::glutin::event::{Event, StartCause};
use glium::glutin::event_loop::ControlFlow;
use glutin::event::VirtualKeyCode;

pub enum Action {
    Stop,
    Continue,
}

pub fn run_loop<F>(event_loop: EventLoop<()>, mut callback: F)
where
    F: 'static + FnMut(&Vec<Event<'_, ()>>),
{
    let mut events_buffer = Vec::new();

    event_loop.run(move |event, _, control_flow| {
        let run_callback = match event.to_static() {
            Some(Event::NewEvents(cause)) => match cause {
                StartCause::ResumeTimeReached { .. } | StartCause::Init => true,
                _ => false,
            },
            Some(event) => {
                events_buffer.push(event);
                false
            }
            None => false,
        };

        if run_callback {
            callback(&Vec::new());
        }

        for event in &events_buffer {
            *control_flow = match event {
                glutin::event::Event::WindowEvent { event, .. } => match event {
                    glutin::event::WindowEvent::CloseRequested => ControlFlow::Exit,
                    glutin::event::WindowEvent::Resized(..) => {
                        //draw();
                        ControlFlow::Poll
                    }
                    glutin::event::WindowEvent::KeyboardInput { input, .. } => {
                        if let glutin::event::ElementState::Pressed = input.state {
                            if let Some(keycode) = input.virtual_keycode {
                                if let VirtualKeyCode::Escape = keycode {
                                    ControlFlow::Exit
                                } else {
                                    match keycode {
                                        VirtualKeyCode::Escape => ControlFlow::Exit,
                                        _ => {
                                            match keycode {
                                                VirtualKeyCode::Return => {
                                                    println!("Pressed");
                                                }
                                                _ => (),
                                            }
                                            ControlFlow::Poll
                                        }
                                    }
                                }
                            } else {
                                ControlFlow::Poll
                            }
                        } else {
                            ControlFlow::Poll
                        }
                    }
                    _ => ControlFlow::Poll,
                },
                _ => ControlFlow::Poll,
            }
        }
        events_buffer.clear();
    });
}
