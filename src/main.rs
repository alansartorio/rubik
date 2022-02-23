#[macro_use]
extern crate glium;

#[allow(unused_imports)]
use glium::{glutin, Surface};
use glium::{
    glutin::{event::VirtualKeyCode, event_loop::ControlFlow},
    index::PrimitiveType,
};

use crate::helper::Action;
mod helper;

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new().with_vsync(true);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();
    let vertex_buffer = {
        #[derive(Copy, Clone)]
        struct Vertex {
            position: [f32; 2],
            color: [f32; 3],
        }

        implement_vertex!(Vertex, position, color);

        glium::VertexBuffer::new(
            &display,
            &[
                Vertex {
                    position: [-0.5, -0.5],
                    color: [0.0, 1.0, 0.0],
                },
                Vertex {
                    position: [-0.5, 0.5],
                    color: [0.0, 0.0, 1.0],
                },
                Vertex {
                    position: [0.5, 0.5],
                    color: [1.0, 0.0, 0.0],
                },
                Vertex {
                    position: [0.5, -0.5],
                    color: [1.0, 0.0, 0.0],
                },
            ],
        )
        .unwrap()
    };
    let per_instance = {
        #[derive(Copy, Clone)]
        struct Attr {
            world_position: [f32; 3],
        }

        implement_vertex!(Attr, world_position);

        let data = (0..10)
            .map(|i| Attr {
                world_position: [i as f32 / 10.0, 0.0, 0.0],
            })
            .collect::<Vec<_>>();
        glium::VertexBuffer::dynamic(&display, &data).unwrap()
    };
    //let index_buffer =
    //glium::IndexBuffer::new(&display, PrimitiveType::TrianglesList, &[0u16, 1, 2]).unwrap();
    let indices = glium::index::NoIndices(PrimitiveType::TriangleFan);

    let program = glium::Program::from_source(
        &display,
        "
            #version 140

            uniform mat4 matrix;
            
            in vec2 position;
            in vec3 color;
            in vec3 world_position;

            out vec3 vColor;

            void main() {
                gl_Position = vec4(vec3(position * 0.1, 1.0) + world_position, 1.0) * matrix;
                vColor = color;
            }
        ",
        "
            #version 140

            in vec3 vColor;
            out vec4 f_color;

            void main() {
                f_color = vec4(vColor, 1.0);
            }
        ",
        None,
    )
    .unwrap();

    let draw = move || {
        let uniforms = uniform! {
            matrix: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0f32],
            ]
        };

        let mut target = display.draw();

        target.clear_color(0.0, 0.0, 0.0, 0.0);
        target
            .draw(
                (&vertex_buffer, per_instance.per_instance().unwrap()),
                &indices,
                &program,
                &uniforms,
                &Default::default(),
            )
            .unwrap();
        target.finish().unwrap();
    };

    
    helper::run_loop(event_loop, move |events| {
        println!("Frame!");
        draw();

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
                                    match keycode {
                                        VirtualKeyCode::Return => {
                                            println!("Pressed");
                                        }
                                        _ => (),
                                    };
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
