use std::{
    collections::HashSet,
    io::{self, Read},
};

use lazy_static::{__Deref, lazy_static};
use rubik::{
    cube::{
        Algorythm, Cube,
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
    Piece::Edge(Front, Right),
    Piece::Edge(Right, Back),
    Piece::Edge(Back, Left),
    Piece::Edge(Left, Front),
    Piece::Edge(Down, Right),
    Piece::Edge(Down, Back),
    Piece::Edge(Down, Left),
    Piece::Edge(Down, Front),
];


fn whitelist(cube: &Cube<3>, pieces: Vec<Piece>) -> Cube<3> {
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

enum SolveStep<'a> {
    Bruteforce(Vec<Piece>, &'a Vec<Algorythm>),
    Fixed(Algorythm),
}

fn main() {
    let basic_moves: Vec<Algorythm> = "R R' L L' F F' U U' D D' B B'"
        .parse::<Algorythm>()
        .unwrap()
        .0
        .iter()
        .map(|&step| Algorythm(vec![step]))
        .collect();

    //let insert_corner: Algorythm = "R U R'".parse().unwrap();
    //let : Algorythm = "R U R'".parse().unwrap();

    let steps: Vec<SolveStep> = vec![
        SolveStep::Bruteforce(vec![EDGES[0]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[1]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[2]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[3]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[4]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[5]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[6]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[7]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[8]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[9]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[10]], &basic_moves),
        SolveStep::Bruteforce(vec![EDGES[11]], &basic_moves),
        //SolveStep::Fixed("z z".parse().unwrap()),
        //SolveStep::Bruteforce(vec![CORNERS[0], EDGES[4]], &basic_moves),
        //SolveStep::Bruteforce(vec![CORNERS[1], EDGES[7]], &basic_moves),
        //SolveStep::Bruteforce(vec![CORNERS[2], EDGES[5]], &basic_moves),
        //SolveStep::Bruteforce(vec![CORNERS[3], EDGES[6]], &basic_moves),
    ];

    let mut input_string = String::new();
    io::stdin().read_to_string(&mut input_string).unwrap();
    let cube: Cube<3> = input_string.parse().unwrap();

    let mut acum_step: Vec<Piece> = vec![];

    for solve_step in steps {
        let algorythm = match solve_step {
            SolveStep::Bruteforce(tiles_to_solve, allowed_moves) => {
                acum_step.append(&mut tiles_to_solve.clone());
                let clone = whitelist(&cube, acum_step.clone());
                solve(&clone, &allowed_moves)
            },
            SolveStep::Fixed(alg) => alg
        };

        cube.apply_algorythm(&algorythm);
        println!("{}", algorythm.to_string());
    }
}
