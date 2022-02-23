use std::time::Duration;
use crate::glutin;
use crate::glutin::event_loop::EventLoop;
use glium::glutin::event::{Event, StartCause, WindowEvent};
use glium::glutin::event_loop::ControlFlow;
use glium::glutin::window::{self, Window};
use glutin::event::VirtualKeyCode;
use std::time::Instant;

pub enum Action {
    Stop,
    Continue,
}

pub fn run_loop<F>(window: &Window, event_loop: EventLoop<()>, mut callback: F)
where
    F: 'static + FnMut(&Vec<Event<'_, ()>>) -> Action,
{
    let mut events_buffer = Vec::new();

    event_loop.run(move |event, _, control_flow| {
        let (run_callback, exit) = match event.to_static() {
            Some(Event::NewEvents(cause)) => match cause {
                StartCause::ResumeTimeReached { .. } | StartCause::Init => (true, false),
                _ => (false, false),
            },
            Some(glutin::event::Event::WindowEvent {
                event: glium::glutin::event::WindowEvent::CloseRequested,
                ..
            }) => (false, true),
            Some(event) => {
                events_buffer.push(event);
                (false, false)
            }
            None => (false, false),
        };

        let action = if run_callback {
            let action = callback(&events_buffer);
            events_buffer.clear();
            //next_frame_time = Instant::now() + Duration::from_nanos(7000000);
            window.request_redraw();

            action
        } else if exit {
            Action::Stop
        } else {
            Action::Continue
        };

        match action {
            Action::Continue => *control_flow = ControlFlow::Poll,
            Action::Stop => *control_flow = ControlFlow::Exit,
        }

        //for event in &events_buffer {
        //*control_flow = match event {
        //glutin::event::Event::WindowEvent { event, .. } => match event {
        //glutin::event::WindowEvent::CloseRequested => ControlFlow::Exit,
        //glutin::event::WindowEvent::Resized(..) => {
        ////draw();
        //ControlFlow::Poll
        //}
        //glutin::event::WindowEvent::KeyboardInput { input, .. } => {
        //if let glutin::event::ElementState::Pressed = input.state {
        //if let Some(keycode) = input.virtual_keycode {
        //if let VirtualKeyCode::Escape = keycode {
        //ControlFlow::Exit
        //} else {
        //match keycode {
        //VirtualKeyCode::Escape => ControlFlow::Exit,
        //_ => {
        //match keycode {
        //VirtualKeyCode::Return => {
        //println!("Pressed");
        //}
        //_ => (),
        //}
        //ControlFlow::Poll
        //}
        //}
        //}
        //} else {
        //ControlFlow::Poll
        //}
        //} else {
        //ControlFlow::Poll
        //}
        //}
        //_ => ControlFlow::Poll,
        //},
        //_ => ControlFlow::Poll,
        //}
        //}
    });
}
