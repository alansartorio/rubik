use glium::glutin;
use glium::glutin::event::Event;
use glium::glutin::event_loop::ControlFlow;
use glium::glutin::event_loop::EventLoop;

pub enum Action {
    Stop,
    Continue,
}

pub fn run_loop<F>(event_loop: EventLoop<()>, mut callback: F)
where
    F: 'static + FnMut(&Vec<Event<'_, ()>>) -> Action,
{
    let mut events_buffer = Vec::new();

    event_loop.run(move |event, _, control_flow| {
        let (run_callback, exit) = match event.to_static() {
            Some(Event::NewEvents(cause)) => match cause {
                //StartCause::ResumeTimeReached { .. } | StartCause::Init => (true, false),
                _ => (false, false),
            },
            Some(glutin::event::Event::MainEventsCleared) => (true, false),
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
    });
}
