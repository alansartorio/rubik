#![feature(type_ascription)]
#[macro_use]
extern crate glium;

use cgmath::{Transform, *};

use std::{
    f32::consts::PI,
    ops::Mul,
    time::{self, SystemTime, UNIX_EPOCH},
};

#[allow(unused_imports)]
use glium::{glutin, Surface};
use glium::{
    glutin::{event::VirtualKeyCode, event_loop::ControlFlow},
    index::PrimitiveType,
};

use crate::helper::Action;
mod helper;
mod colors {
    type Color = [f32; 3];
    pub static RED: Color = [1.0, 0.0, 0.0];
    pub static GREEN: Color = [0.0, 1.0, 0.0];
    pub static BLUE: Color = [0.0, 0.0, 1.0];
    pub static YELLOW: Color = [1.0, 1.0, 0.0];
    pub static WHITE: Color = [1.0, 1.0, 1.0];
    pub static ORANGE: Color = [1.0, 0.3, 0.0];
}

fn main() {
    let event_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new();
    let cb = glutin::ContextBuilder::new()
        .with_depth_buffer(24)
        .with_vsync(true);
    let display = glium::Display::new(wb, cb, &event_loop).unwrap();
    let vertex_buffer = {
        #[derive(Copy, Clone)]
        struct Vertex {
            position: [f32; 2],
        }

        implement_vertex!(Vertex, position);

        glium::VertexBuffer::new(
            &display,
            &[
                Vertex {
                    position: [-0.5, -0.5],
                },
                Vertex {
                    position: [-0.5, 0.5],
                },
                Vertex {
                    position: [0.5, 0.5],
                },
                Vertex {
                    position: [0.5, -0.5],
                },
            ],
        )
        .unwrap()
    };
    let per_instance = {
        #[derive(Copy, Clone, Debug)]
        struct Attr {
            model_to_world: [[f32; 4]; 4],
            color: [f32; 3],
        }

        implement_vertex!(Attr, model_to_world, color);

        //let colors = [
        //[colors::RED, colors::ORANGE, colors::WHITE],
        //[colors::GREEN, colors::YELLOW, colors::BLUE],
        //[colors::RED, colors::ORANGE, colors::WHITE],
        //];
        let colors = [
            colors::RED,
            colors::ORANGE,
            colors::WHITE,
            colors::YELLOW,
            colors::BLUE,
            colors::GREEN,
        ];
        const HALF_PI: f32 = PI / 2.;
        let rotations = [
            Matrix4::from_angle_x(Rad(HALF_PI)),
            Matrix4::from_angle_x(Rad(-HALF_PI)),
            Matrix4::from_angle_y(Rad(HALF_PI)),
            Matrix4::from_angle_y(Rad(-HALF_PI)),
            Matrix4::identity(),
            Matrix4::from_angle_x(Rad(PI)),
        ];
        let data = Iterator::zip(colors.iter(), rotations)
            .map(|(color, rotation)| {
                (-1i16..=1)
                    .map(|x| {
                        (-1i16..=1)
                            .map(|y| Attr {
                                model_to_world: (rotation
                                    .mul(Matrix4::from_translation(Vector3::new(
                                        x as f32 * 0.15,
                                        y as f32 * 0.15,
                                        -0.15 * 3. / 2.,
                                    )))
                                    .into():
                                    [[f32; 4]; 4]),
                                color: *color,
                            })
                            .collect::<Vec<_>>()
                    })
                    .flatten()
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect::<Vec<_>>();
        //let data = [Attr {
        //color: colors::RED,
        //model_to_world: matrix::translate(0.0, 0.0, -2.0),
        //}];
        println!("{:?}", data.first());
        glium::VertexBuffer::dynamic(&display, &data).unwrap()
    };
    //let index_buffer =
    //glium::IndexBuffer::new(&display, PrimitiveType::TrianglesList, &[0u16, 1, 2]).unwrap();
    let indices = glium::index::NoIndices(PrimitiveType::TriangleFan);

    let program = glium::Program::from_source(
        &display,
        "
            #version 140

            uniform mat4 projection;
            uniform mat4 view;
            
            in vec2 position;
            in vec3 color;
            in mat4 model_to_world;

            out vec3 vColor;

            void main() {
                gl_Position = projection * view * model_to_world * vec4(position * 0.1, 0.0, 1.0);// * matrix;
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

    let timer = SystemTime::now();
    let draw = move || {
        let (width, height) = display.get_framebuffer_dimensions();
        let rotation = timer.elapsed().unwrap().as_secs_f32();
        println!("{}", &rotation);
        //let view: [[f32; 4]; 4] = Matrix4::from_nonuniform_scale(1000.0/width as f32, 1000.0/height as f32, 1.).into();
        let view: [[f32; 4]; 4] = Matrix4::look_at_rh(
            Matrix4::from_angle_y(Rad(rotation)).transform_point(Point3::new(0., 2., -2.)),
            Point3::new(0., 0., 0.),
            Vector3::new(0., 1., 0.),
        )
        .into();
        let projection: [[f32; 4]; 4] = ortho(
            -(width as f32) / 1000.0,
            width as f32 / 1000.0,
            -(height as f32) / 1000.0,
            height as f32 / 1000.0,
            0.1,
            100.0,
        )
        .into();
        let uniforms = uniform! {
            //view: matrix::rotate_y(rotation / 5.),
            view: view,
                ////.rotate(rotation, 0., 0.)
                //.orthographic(width as f32, height as f32 , 1.0)
                ////[matrix::scale(1000.0/width as f32, 1000.0/height as f32, 1.0), matrix::rotate_x(angle)]
                                ////.iter().fold(matrix::identity(), |a, &b| matrix::multiply(a, b)),
            projection: projection,
            //projection: Transform::new()
                //.translate(0., 0., 2.)
                //.rotate(rotation / 10., rotation, 0.)
                //.orthographic(width as f32, height as f32 , 10.0)
                //[matrix::scale(1000.0/width as f32, 1000.0/height as f32, 1.0), matrix::rotate_x(angle)]
                                //.iter().fold(matrix::identity(), |a, &b| matrix::multiply(a, b)),
        };

        let mut target = display.draw();

        target.clear_color_and_depth((0.0, 0.0, 0.0, 0.0), 1.0);
        target
            .draw(
                (&vertex_buffer, per_instance.per_instance().unwrap()),
                &indices,
                &program,
                &uniforms,
                &glium::DrawParameters {
                    depth: glium::Depth {
                        test: glium::DepthTest::IfLess,
                        write: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            )
            .unwrap();
        target.finish().unwrap();
    };

    helper::run_loop(event_loop, move |events| {
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
