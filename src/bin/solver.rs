use std::{
    collections::HashSet,
    io::{self, Read},
};

use lazy_static::{__Deref, lazy_static};
use rubik::{
    cube::{
        Cube,
        FaceId::{self, *},
    },
    solver::solve,
};
use strum::IntoEnumIterator;

#[derive(Clone, Copy)]
enum Piece {
    Corner(FaceId, FaceId, FaceId),
    Edge(FaceId, FaceId),
}

const CORNERS: [Piece; 8] = [
    Piece::Corner(Up, Front, Right),
    Piece::Corner(Up, Front, Left),
    Piece::Corner(Up, Back, Right),
    Piece::Corner(Up, Back, Left),
    Piece::Corner(Down, Front, Right),
    Piece::Corner(Down, Front, Left),
    Piece::Corner(Down, Back, Right),
    Piece::Corner(Down, Back, Left),
];

const EDGES: [Piece; 12] = [
    Piece::Edge(Up, Right),
    Piece::Edge(Up, Back),
    Piece::Edge(Up, Left),
    Piece::Edge(Up, Front),
    Piece::Edge(Down, Right),
    Piece::Edge(Down, Back),
    Piece::Edge(Down, Left),
    Piece::Edge(Down, Front),
    Piece::Edge(Front, Right),
    Piece::Edge(Right, Back),
    Piece::Edge(Back, Left),
    Piece::Edge(Left, Front),
];

lazy_static! {
    static ref STEPS: [Vec<Piece>; 8] = [
        vec![EDGES[0]],
        vec![EDGES[1]],
        vec![EDGES[2]],
        vec![EDGES[3]],
        vec![CORNERS[0]],
        vec![CORNERS[1]],
        vec![CORNERS[2]],
        vec![CORNERS[3]],
    ];
}

fn whitelist(cube: &Cube, pieces: Vec<Piece>) -> Cube {
    let shown_tiles = pieces
        .iter()
        .cloned()
        .flat_map(|piece| match piece {
            Piece::Edge(f1, f2) => cube.find_edge([f1, f2]).unwrap().to_vec(),
            Piece::Corner(f1, f2, f3) => cube.find_corner([f1, f2, f3]).unwrap().to_vec(),
        })
        .chain(vec![
            (Up, 1, 1),
            (Down, 1, 1),
            (Front, 1, 1),
            (Back, 1, 1),
            (Right, 1, 1),
            (Left, 1, 1),
        ])
        .collect::<HashSet<_>>();

    let all_stickers = FaceId::iter()
        .flat_map(|f| (0u8..3).map(move |y| (f, y)))
        .flat_map(|(f, y)| (0u8..3).map(move |x| (f, y, x)))
        .collect::<HashSet<_>>();

    let hidden_stickers = all_stickers.difference(&shown_tiles).cloned();

    let cube = cube.clone();
    for (face, y, x) in hidden_stickers {
        cube.hide_sticker(face, y, x);
    }

    cube
}

fn main() {
    let mut input_string = String::new();
    io::stdin().read_to_string(&mut input_string).unwrap();
    let cube: Cube = input_string.parse().unwrap();
    //let cube = Cube::solved();
    //let scramble = Algorythm::random(20);
    //println!("{}", scramble.to_string());
    //cube.apply_algorythm(&scramble);

    let mut acum_step: Vec<Piece> = vec![];

    for step in STEPS.deref() {
        acum_step.append(&mut step.clone());
        let clone = whitelist(&cube, acum_step.clone());
        let step_solution = solve(&clone);

        cube.apply_algorythm(&step_solution);
        println!("{}", step_solution.to_string());
    }
}
