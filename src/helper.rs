use glium::winit::application::ApplicationHandler;
use glium::winit::event::WindowEvent;
use glium::winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use glium::winit::window::{Window, WindowId};

pub enum Action {
    Stop,
    Continue,
}

struct LoopApp<F>
where
    F: FnMut(&Vec<WindowEvent>) -> Action,
{
    window: Window,
    events_buffer: Vec<WindowEvent>,
    callback: F,
}

impl<F> ApplicationHandler for LoopApp<F>
where
    F: FnMut(&Vec<WindowEvent>) -> Action,
{
    fn resumed(&mut self, _event_loop: &ActiveEventLoop) {}

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::RedrawRequested => {
                let action = (self.callback)(&self.events_buffer);
                self.events_buffer.clear();

                if let Action::Stop = action {
                    event_loop.exit();
                }
            }
            event => self.events_buffer.push(event),
        }
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        self.window.request_redraw();
    }
}

pub fn run_loop<F>(event_loop: EventLoop<()>, window: Window, callback: F)
where
    F: FnMut(&Vec<WindowEvent>) -> Action,
{
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = LoopApp {
        window,
        events_buffer: Vec::new(),
        callback,
    };

    event_loop.run_app(&mut app).unwrap();
}
