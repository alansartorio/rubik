use bimap::BiMap;
use enum_map::{enum_map, Enum, EnumMap};
use rand::seq::SliceRandom;
use std::char;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt::{Debug, Display};
use std::iter::FromIterator;
use std::ops::Neg;
use std::str::FromStr;
use std::{cell::RefCell, convert::TryInto, rc::Rc};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Enum)]
enum Sides {
    Left,
    Right,
    Top,
    Bottom,
}
use Sides::*;

#[derive(Debug, Clone)]
pub struct FaceIdParseError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StickerType(pub Option<FaceId>);

#[derive(Debug, Enum, EnumIter, PartialEq, Eq, Clone, Copy, Hash)]
pub enum FaceId {
    Up,
    Down,
    Right,
    Left,
    Front,
    Back,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MiddleRotation {
    M,
    E,
    S,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CubeRotation {
    X,
    Y,
    Z,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Movement {
    Rotation(FaceId),
    DoubleRotation(FaceId),
    MiddleRotation(MiddleRotation),
    CubeRotation(CubeRotation),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Step {
    pub movement: Movement,
    pub count: i8,
}

impl Neg for Step {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            movement: self.movement,
            count: -self.count,
        }
    }
}

lazy_static! {
    static ref MAPPING: BiMap<&'static str, Step> = {
        let mut m = BiMap::new();
        m.insert("R", Step::new(Movement::Rotation(FaceId::Right), 1));
        m.insert("R2", Step::new(Movement::Rotation(FaceId::Right), 2));
        m.insert("R'", Step::new(Movement::Rotation(FaceId::Right), -1));
        m.insert("L", Step::new(Movement::Rotation(FaceId::Left), 1));
        m.insert("L2", Step::new(Movement::Rotation(FaceId::Left), 2));
        m.insert("L'", Step::new(Movement::Rotation(FaceId::Left), -1));
        m.insert("U", Step::new(Movement::Rotation(FaceId::Up), 1));
        m.insert("U2", Step::new(Movement::Rotation(FaceId::Up), 2));
        m.insert("U'", Step::new(Movement::Rotation(FaceId::Up), -1));
        m.insert("D", Step::new(Movement::Rotation(FaceId::Down), 1));
        m.insert("D2", Step::new(Movement::Rotation(FaceId::Down), 2));
        m.insert("D'", Step::new(Movement::Rotation(FaceId::Down), -1));
        m.insert("F", Step::new(Movement::Rotation(FaceId::Front), 1));
        m.insert("F2", Step::new(Movement::Rotation(FaceId::Front), 2));
        m.insert("F'", Step::new(Movement::Rotation(FaceId::Front), -1));
        m.insert("B", Step::new(Movement::Rotation(FaceId::Back), 1));
        m.insert("B2", Step::new(Movement::Rotation(FaceId::Back), 2));
        m.insert("B'", Step::new(Movement::Rotation(FaceId::Back), -1));
        m.insert("r", Step::new(Movement::DoubleRotation(FaceId::Right), 1));
        m.insert("r'", Step::new(Movement::DoubleRotation(FaceId::Right), -1));
        m.insert("l", Step::new(Movement::DoubleRotation(FaceId::Left), 1));
        m.insert("l'", Step::new(Movement::DoubleRotation(FaceId::Left), -1));
        m.insert("u", Step::new(Movement::DoubleRotation(FaceId::Up), 1));
        m.insert("u'", Step::new(Movement::DoubleRotation(FaceId::Up), -1));
        m.insert("d", Step::new(Movement::DoubleRotation(FaceId::Down), 1));
        m.insert("d'", Step::new(Movement::DoubleRotation(FaceId::Down), -1));
        m.insert("f", Step::new(Movement::DoubleRotation(FaceId::Front), 1));
        m.insert("f'", Step::new(Movement::DoubleRotation(FaceId::Front), -1));
        m.insert("b", Step::new(Movement::DoubleRotation(FaceId::Back), 1));
        m.insert("b'", Step::new(Movement::DoubleRotation(FaceId::Back), -1));
        m.insert(
            "M",
            Step::new(Movement::MiddleRotation(MiddleRotation::M), 1),
        );
        m.insert(
            "M'",
            Step::new(Movement::MiddleRotation(MiddleRotation::M), -1),
        );
        m.insert(
            "E",
            Step::new(Movement::MiddleRotation(MiddleRotation::E), 1),
        );
        m.insert(
            "E'",
            Step::new(Movement::MiddleRotation(MiddleRotation::E), -1),
        );
        m.insert(
            "S",
            Step::new(Movement::MiddleRotation(MiddleRotation::S), 1),
        );
        m.insert(
            "S'",
            Step::new(Movement::MiddleRotation(MiddleRotation::S), -1),
        );
        m.insert("x", Step::new(Movement::CubeRotation(CubeRotation::X), 1));
        m.insert("x'", Step::new(Movement::CubeRotation(CubeRotation::X), -1));
        m.insert("y", Step::new(Movement::CubeRotation(CubeRotation::Y), 1));
        m.insert("y'", Step::new(Movement::CubeRotation(CubeRotation::Y), -1));
        m.insert("z", Step::new(Movement::CubeRotation(CubeRotation::Z), 1));
        m.insert("z'", Step::new(Movement::CubeRotation(CubeRotation::Z), -1));
        m
    };
}

impl FromStr for Step {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        MAPPING.get_by_left(s).map(|&m| m).ok_or("Invalid Step String".into())
    }
}

impl ToString for Step {
    fn to_string(&self) -> String {
        (*MAPPING.get_by_right(self).unwrap()).to_owned()
    }
}

impl Step {
    pub fn new(movement: Movement, count: i8) -> Step {
        Step { movement, count }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Algorythm(pub Vec<Step>);

impl ToString for Algorythm {
    fn to_string(&self) -> String {
        self.0.iter().map(ToString::to_string).collect::<Vec<_>>().join(" ")
    }
}

impl Algorythm {
    pub fn random(depth: u8) -> Algorythm {
        let moves = [
            FaceId::Up,
            FaceId::Down,
            FaceId::Right,
            FaceId::Left,
            FaceId::Front,
            FaceId::Back,
        ];

        let mut steps = vec![];
        let mut rng = rand::thread_rng();
        for _ in 0..depth {
            let choice = *moves.choose(&mut rng).unwrap();
            
            steps.push(Step::new(Movement::Rotation(choice), 1));
        }

        Algorythm(steps)
    }
}

impl Neg for Algorythm {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Algorythm(self.0.iter().rev().cloned().map(Neg::neg).collect())
    }
}

impl FromStr for Algorythm {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split(' ')
            .filter(|s| !s.is_empty())
            .map(str::parse::<Step>)
            .collect::<Result<Vec<Step>, Self::Err>>()
            .map(Algorythm)
    }
}

fn normalize_rotation(rotation: i8) -> u8 {
    rotation.rem_euclid(4) as u8
}

impl Display for StickerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let l = match self {
            StickerType(Some(face)) => match face {
                FaceId::Up => 'u',
                FaceId::Down => 'd',
                FaceId::Right => 'r',
                FaceId::Left => 'l',
                FaceId::Front => 'f',
                FaceId::Back => 'b',
            },
            StickerType(None) => '0',
        };
        write!(f, "{}", l)
    }
}
impl TryFrom<char> for StickerType {
    type Error = FaceIdParseError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'u' => Ok(StickerType(Some(FaceId::Up))),
            'd' => Ok(StickerType(Some(FaceId::Down))),
            'r' => Ok(StickerType(Some(FaceId::Right))),
            'l' => Ok(StickerType(Some(FaceId::Left))),
            'f' => Ok(StickerType(Some(FaceId::Front))),
            'b' => Ok(StickerType(Some(FaceId::Back))),
            '0' => Ok(StickerType(None)),
            _ => Err(FaceIdParseError),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct FaceData {
    tiles: [[StickerType; 3]; 3],
}
impl FaceData {
    fn new(tiles: [[StickerType; 3]; 3]) -> FaceData {
        FaceData { tiles }
    }

    fn rotate(&mut self) -> &mut Self {
        let tiles = &mut self.tiles;
        let tmp = tiles[0][0];
        tiles[0][0] = tiles[0][2];
        tiles[0][2] = tiles[2][2];
        tiles[2][2] = tiles[2][0];
        tiles[2][0] = tmp;

        let tmp = tiles[0][1];
        tiles[0][1] = tiles[1][2];
        tiles[1][2] = tiles[2][1];
        tiles[2][1] = tiles[1][0];
        tiles[1][0] = tmp;

        self
    }

    fn flatten_stickers(&self) -> [StickerType; 9] {
        let tiles = self.tiles;
        [
            tiles[0][0],
            tiles[0][1],
            tiles[0][2],
            tiles[1][0],
            tiles[1][1],
            tiles[1][2],
            tiles[2][0],
            tiles[2][1],
            tiles[2][2],
        ]
    }

    fn is_solved(&self) -> bool {
        let stickers = self.flatten_stickers();
        let mut iter = stickers.iter().filter_map(|s| s.0);
        let sticker = iter.next().unwrap();

        iter.all(|s| s == sticker)
    }
}
impl FromStr for FaceData {
    type Err = FaceIdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FaceData::new(
            s.split('\n')
                .map(|row| {
                    row.trim()
                        .chars()
                        .map(|c| StickerType::try_from(c).unwrap())
                        .collect::<Vec<_>>()
                        .try_into()
                        .unwrap()
                })
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        ))
    }
}

impl Debug for FaceData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}{}{}\n{}{}{}\n{}{}{}",
            self.tiles[0][0],
            self.tiles[0][1],
            self.tiles[0][2],
            self.tiles[1][0],
            self.tiles[1][1],
            self.tiles[1][2],
            self.tiles[2][0],
            self.tiles[2][1],
            self.tiles[2][2]
        )
    }
}

struct Face {
    sides: EnumMap<Sides, FaceBorderView>,
    face_data: Rc<RefCell<FaceData>>,
}

impl Debug for Face {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", RefCell::borrow(&self.face_data))
    }
}
impl Face {
    fn rotate(&self, cw_turns: i8) {
        let ccw_turns = normalize_rotation(4 - cw_turns);
        for _ in 0..ccw_turns {
            RefCell::borrow_mut(&self.face_data).rotate();
            let tmp = self.sides[Top].get();
            self.sides[Top].set(self.sides[Right].get());
            self.sides[Right].set(self.sides[Bottom].get());
            self.sides[Bottom].set(self.sides[Left].get());
            self.sides[Left].set(tmp);
        }
    }
}

struct FaceBorderView {
    face: Rc<RefCell<FaceData>>,
    side: Sides,
}
impl FaceBorderView {
    fn new(face: Rc<RefCell<FaceData>>, side: Sides) -> FaceBorderView {
        FaceBorderView { face, side }
    }
}
impl FaceBorderView {
    fn get(&self) -> [StickerType; 3] {
        //let face: FaceData = self.face.borrow().tiles;
        let tiles = RefCell::borrow(&self.face).tiles;
        match self.side {
            Sides::Top => [tiles[0][0], tiles[0][1], tiles[0][2]],
            Sides::Right => [tiles[0][2], tiles[1][2], tiles[2][2]],
            Sides::Bottom => [tiles[2][2], tiles[2][1], tiles[2][0]],
            Sides::Left => [tiles[2][0], tiles[1][0], tiles[0][0]],
        }
    }
    fn set(&self, row: [StickerType; 3]) {
        //let face: FaceData = self.face.borrow().tiles;
        let tiles = &mut RefCell::borrow_mut(&self.face).tiles;
        match self.side {
            Sides::Top => {
                tiles[0][0] = row[0];
                tiles[0][1] = row[1];
                tiles[0][2] = row[2];
            }
            Sides::Right => {
                tiles[0][2] = row[0];
                tiles[1][2] = row[1];
                tiles[2][2] = row[2];
            }
            Sides::Bottom => {
                tiles[2][2] = row[0];
                tiles[2][1] = row[1];
                tiles[2][0] = row[2];
            }
            Sides::Left => {
                tiles[2][0] = row[0];
                tiles[1][0] = row[1];
                tiles[0][0] = row[2];
            }
        }
    }
}

pub struct Cube {
    faces: EnumMap<FaceId, Face>,
}

impl Clone for Cube {
    fn clone(&self) -> Self {
        Self::new(enum_map! {
            id => *RefCell::borrow(&self.faces[id].face_data)
        })
    }
}

impl Debug for Cube {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n{:?}\n",
            self.faces[FaceId::Up],
            self.faces[FaceId::Down],
            self.faces[FaceId::Right],
            self.faces[FaceId::Left],
            self.faces[FaceId::Front],
            self.faces[FaceId::Back],
        )
    }
}

impl Cube {
    fn new(faces: EnumMap<FaceId, FaceData>) -> Cube {
        Cube {
            faces: {
                let up_data = Rc::new(RefCell::new(faces[FaceId::Up]));
                let down_data = Rc::new(RefCell::new(faces[FaceId::Down]));
                let left_data = Rc::new(RefCell::new(faces[FaceId::Left]));
                let right_data = Rc::new(RefCell::new(faces[FaceId::Right]));
                let front_data = Rc::new(RefCell::new(faces[FaceId::Front]));
                let back_data = Rc::new(RefCell::new(faces[FaceId::Back]));

                enum_map! {
                    FaceId::Up => Face {
                        face_data: Rc::clone(&up_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&back_data), Top),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&front_data), Top),
                            Sides::Left => FaceBorderView::new(Rc::clone(&left_data), Top),
                            Sides::Right => FaceBorderView::new(Rc::clone(&right_data), Top),
                        },
                    },
                    FaceId::Down => Face {
                        face_data: Rc::clone(&down_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&front_data), Bottom),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&back_data), Bottom),
                            Sides::Left => FaceBorderView::new(Rc::clone(&left_data), Bottom),
                            Sides::Right => FaceBorderView::new(Rc::clone(&right_data), Bottom),
                        },
                    },
                    FaceId::Left => Face {
                        face_data: Rc::clone(&left_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Left),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Left),
                            Sides::Left => FaceBorderView::new(Rc::clone(&back_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&front_data), Left),
                        },
                    },
                    FaceId::Right => Face {
                        face_data: Rc::clone(&right_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Right),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Right),
                            Sides::Left => FaceBorderView::new(Rc::clone(&front_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&back_data), Left),
                        },
                    },
                    FaceId::Back => Face {
                        face_data: Rc::clone(&back_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Top),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Bottom),
                            Sides::Left => FaceBorderView::new(Rc::clone(&right_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&left_data), Left),
                        },
                    },
                    FaceId::Front => Face {
                        face_data: Rc::clone(&front_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Bottom),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Top),
                            Sides::Left => FaceBorderView::new(Rc::clone(&left_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&right_data), Left),
                        },
                    },
                }
            },
        }
    }

    pub fn is_solved(&self) -> bool {
        FaceId::iter().all(|face| self.get_face(face).is_solved())
    }

    pub fn parse(data: &str) -> Cube {
        let faces: [FaceData; 6] = data
            .trim()
            .split("\n\n")
            .map(|face| face.parse::<FaceData>().unwrap())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap();

        Cube::new(enum_map! {
            FaceId::Up => faces[0],
            FaceId::Down => faces[1],
            FaceId::Right => faces[2],
            FaceId::Left => faces[3],
            FaceId::Front => faces[4],
            FaceId::Back => faces[5],
        })
    }

    pub fn find_corner(&self, corner: [FaceId; 3]) -> Option<[(FaceId, u8, u8); 3]> {
        [
            [
                (FaceId::Front, 0u8, 0u8),
                (FaceId::Up, 2, 0),
                (FaceId::Left, 0, 2),
            ],
            [
                (FaceId::Front, 0, 2),
                (FaceId::Up, 2, 2),
                (FaceId::Right, 0, 0),
            ],
            [
                (FaceId::Front, 2, 2),
                (FaceId::Down, 0, 2),
                (FaceId::Right, 2, 0),
            ],
            [
                (FaceId::Front, 2, 0),
                (FaceId::Down, 0, 0),
                (FaceId::Left, 2, 2),
            ],
            [
                (FaceId::Back, 0, 0),
                (FaceId::Up, 0, 2),
                (FaceId::Right, 0, 2),
            ],
            [
                (FaceId::Back, 0, 2),
                (FaceId::Up, 0, 0),
                (FaceId::Left, 0, 0),
            ],
            [
                (FaceId::Back, 2, 2),
                (FaceId::Down, 2, 0),
                (FaceId::Left, 2, 0),
            ],
            [
                (FaceId::Back, 2, 0),
                (FaceId::Down, 2, 2),
                (FaceId::Right, 2, 2),
            ],
        ]
        .iter()
        .cloned()
        .find(|tiles| {
            if let Some(tiles) = tiles
                .iter()
                .cloned()
                .map(|(face, y, x)| {
                    RefCell::borrow(&self.faces[face].face_data).tiles[y as usize][x as usize].0
                })
                .collect::<Option<Vec<FaceId>>>()
            {
                HashSet::<_>::from_iter(tiles) == HashSet::from_iter(corner)
            } else {
                false
            }
        })
    }

    pub fn find_edge(&self, edge: [FaceId; 2]) -> Option<[(FaceId, u8, u8); 2]> {
        macro_rules! top {
            ($face: ident) => {
                (FaceId::$face, 0, 1)
            };
        }
        macro_rules! bottom {
            ($face: ident) => {
                (FaceId::$face, 2, 1)
            };
        }
        macro_rules! left {
            ($face: ident) => {
                (FaceId::$face, 1, 0)
            };
        }
        macro_rules! right {
            ($face: ident) => {
                (FaceId::$face, 1, 2)
            };
        }
        [
            [top!(Back), top!(Up)],
            [left!(Back), right!(Right)],
            [right!(Back), left!(Left)],
            [bottom!(Back), bottom!(Down)],
            [top!(Front), bottom!(Up)],
            [left!(Front), right!(Left)],
            [right!(Front), left!(Right)],
            [bottom!(Front), top!(Down)],
            [right!(Up), top!(Right)],
            [bottom!(Right), right!(Down)],
            [left!(Down), bottom!(Left)],
            [top!(Left), left!(Up)],
        ]
        .iter()
        .cloned()
        .find(|tiles| {
            if let Some(tiles) = tiles
                .iter()
                .cloned()
                .map(|(face, y, x)| {
                    RefCell::borrow(&self.faces[face].face_data).tiles[y as usize][x as usize].0
                })
                .collect::<Option<Vec<FaceId>>>()
            {
                HashSet::<_>::from_iter(tiles) == HashSet::from_iter(edge)
            } else {
                false
            }
        })
    }

    pub fn hide_sticker(&self, face: FaceId, y: u8, x: u8) {
        let mut face = RefCell::borrow_mut(&self.faces[face].face_data);
        face.tiles[y as usize][x as usize] = StickerType(None);
    }

    pub fn solve(&self) {
        for face in self.faces.values() {
            let mut face = RefCell::borrow_mut(&face.face_data);
            if face.tiles[1][1].0.is_some() {
                let center = face.tiles[1][1];
                face.tiles[0][0] = center;
                face.tiles[0][1] = center;
                face.tiles[0][2] = center;
                face.tiles[1][0] = center;
                face.tiles[1][1] = center;
                face.tiles[1][2] = center;
                face.tiles[2][0] = center;
                face.tiles[2][1] = center;
                face.tiles[2][2] = center;
            }
        }
    }

    pub fn solved() -> Cube {
        Cube::parse(
            "
            uuu\nuuu\nuuu\n
            ddd\nddd\nddd\n
            rrr\nrrr\nrrr\n
            lll\nlll\nlll\n
            fff\nfff\nfff\n
            bbb\nbbb\nbbb\n
        ",
        )
    }

    pub fn scramble_count(&self, move_count: u8) {
        self.apply_algorythm(&Algorythm::random(move_count));
    }

    pub fn scramble(&self) {
        self.scramble_count(100);
    }

    pub fn get_face(&self, face: FaceId) -> FaceData {
        *RefCell::borrow(&self.faces[face].face_data)
    }

    pub fn apply_step(&self, step: Step) {
        let count = step.count;
        match step.movement {
            Movement::Rotation(rot) => self.rotate_face(rot, count),
            Movement::DoubleRotation(rot) => self.rotate_double(rot, count),
            Movement::MiddleRotation(rot) => self.rotate_middle(rot, count),
            Movement::CubeRotation(rot) => self.rotate_cube(rot, count),
        }
    }

    pub fn apply_algorythm(&self, algorythm: &Algorythm) {
        for &step in &algorythm.0 {
            self.apply_step(step);
        }
    }

    fn rotate_face(&self, face: FaceId, cw_turns: i8) {
        self.faces[face].rotate(cw_turns);
    }

    fn rotate_double(&self, face: FaceId, cw_turns: i8) {
        let cw_turns = normalize_rotation(cw_turns);
        self.faces[face].rotate(cw_turns as i8);

        let (middle, count): (_, i8) = match face {
            FaceId::Up => (MiddleRotation::E, -1),
            FaceId::Down => (MiddleRotation::E, 1),
            FaceId::Right => (MiddleRotation::M, -1),
            FaceId::Left => (MiddleRotation::M, 1),
            FaceId::Front => (MiddleRotation::S, -1),
            FaceId::Back => (MiddleRotation::S, 1),
        };
        self.rotate_middle(middle, count * cw_turns as i8);
    }

    fn rotate_cube(&self, rotation: CubeRotation, count: i8) {
        macro_rules! borrow_mut {
            ($face: expr) => {
                RefCell::borrow_mut(&self.faces[$face].face_data)
            };
        }
        macro_rules! borrow {
            ($face: expr) => {
                RefCell::borrow(&self.faces[$face].face_data)
            };
        }

        macro_rules! copy {
            ($to: expr, $from: expr) => {
                *borrow_mut!($to) = *borrow!($from)
            };
        }

        macro_rules! rotate {
            ($face1: expr, $face2: expr, $face3: expr, $face4: expr) => {{
                let tmp = *borrow!($face4);
                copy!($face4, $face3);
                copy!($face3, $face2);
                copy!($face2, $face1);
                *borrow_mut!($face1) = tmp;
            }};
        }

        for _ in 0..count.abs() {
            match rotation {
                CubeRotation::X if count > 0 => {
                    borrow_mut!(FaceId::Right).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Left).rotate();
                    borrow_mut!(FaceId::Back).rotate().rotate();
                    rotate!(FaceId::Up, FaceId::Back, FaceId::Down, FaceId::Front);
                    borrow_mut!(FaceId::Back).rotate().rotate();
                }
                CubeRotation::X if count < 0 => {
                    borrow_mut!(FaceId::Right).rotate();
                    borrow_mut!(FaceId::Left).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Back).rotate().rotate();
                    rotate!(FaceId::Up, FaceId::Front, FaceId::Down, FaceId::Back);
                    borrow_mut!(FaceId::Back).rotate().rotate();
                }
                CubeRotation::Y if count > 0 => {
                    borrow_mut!(FaceId::Up).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Down).rotate();
                    rotate!(FaceId::Front, FaceId::Left, FaceId::Back, FaceId::Right);
                }
                CubeRotation::Y if count < 0 => {
                    borrow_mut!(FaceId::Up).rotate();
                    borrow_mut!(FaceId::Down).rotate().rotate().rotate();
                    rotate!(FaceId::Front, FaceId::Right, FaceId::Back, FaceId::Left);
                }
                CubeRotation::Z if count > 0 => {
                    borrow_mut!(FaceId::Front).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Back).rotate();
                    borrow_mut!(FaceId::Left).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Right).rotate();
                    borrow_mut!(FaceId::Down).rotate().rotate();
                    rotate!(FaceId::Up, FaceId::Right, FaceId::Down, FaceId::Left);
                    borrow_mut!(FaceId::Down).rotate().rotate();
                    borrow_mut!(FaceId::Right).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Left).rotate();
                }
                CubeRotation::Z if count < 0 => {
                    borrow_mut!(FaceId::Front).rotate();
                    borrow_mut!(FaceId::Back).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Left).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Right).rotate();
                    borrow_mut!(FaceId::Down).rotate().rotate();
                    rotate!(FaceId::Up, FaceId::Left, FaceId::Down, FaceId::Right);
                    borrow_mut!(FaceId::Down).rotate().rotate();
                    borrow_mut!(FaceId::Right).rotate().rotate().rotate();
                    borrow_mut!(FaceId::Left).rotate();
                }
                _ if count == 0 => (),
                _ => unreachable!(),
            }
        }
    }

    fn rotate_middle(&self, rotation: MiddleRotation, count: i8) {
        let cw_turns = normalize_rotation(count);
        for _ in 0..cw_turns {
            match rotation {
                MiddleRotation::M => {
                    self.rotate_face(FaceId::Left, -1);
                    self.rotate_face(FaceId::Right, 1);
                    self.rotate_cube(CubeRotation::X, -1);
                }
                MiddleRotation::E => {
                    self.rotate_face(FaceId::Up, 1);
                    self.rotate_face(FaceId::Down, -1);
                    self.rotate_cube(CubeRotation::Y, -1);
                }
                MiddleRotation::S => {
                    self.rotate_face(FaceId::Back, 1);
                    self.rotate_face(FaceId::Front, -1);
                    self.rotate_cube(CubeRotation::Z, -1);
                }
            };
        }
    }

    pub fn flatten_stickers(&self) -> Vec<StickerType> {
        [
            FaceId::Up,
            FaceId::Down,
            FaceId::Right,
            FaceId::Left,
            FaceId::Front,
            FaceId::Back,
        ]
        .map(|face| self.get_face(face))
        .iter()
        .flat_map(|face| face.flatten_stickers().to_vec())
        .collect::<Vec<_>>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cube_parse() {
        let cube = Cube::solved();
        cube.rotate_face(FaceId::Up, 1);
        assert_eq!(
            cube.get_face(FaceId::Up),
            FaceData::from_str("uuu\nuuu\nuuu").unwrap()
        );
        assert_eq!(
            cube.get_face(FaceId::Front),
            FaceData::from_str("rrr\nfff\nfff").unwrap()
        );
    }

    #[test]
    fn parse_algorythms() {
        let algorythm: Algorythm = " R R'   L L'   ".parse().unwrap();

        assert_eq!(
            algorythm,
            Algorythm(vec![
                Step::new(Movement::Rotation(FaceId::Right), 1),
                Step::new(Movement::Rotation(FaceId::Right), -1),
                Step::new(Movement::Rotation(FaceId::Left), 1),
                Step::new(Movement::Rotation(FaceId::Left), -1),
            ])
        );
    }

    #[test]
    fn algorythm_and_reverse() {
        let algorythm: Algorythm =
            "B2 D' B L' F2 U D' L' D R2 L' F2 B2 L2 D F' L2 U' B2 F D2 R D L2 U"
                .parse()
                .unwrap();

        let reversed = -algorythm.clone();

        let cube = Cube::solved();

        cube.apply_algorythm(&algorythm);
        assert!(!cube.is_solved());

        cube.apply_algorythm(&reversed);
        assert!(cube.is_solved());
    }

    #[test]
    fn find_corner() {
        let cube = Cube::solved();

        let corner = cube.find_corner([FaceId::Up, FaceId::Right, FaceId::Front]);
        assert!(corner.is_some());
        let corner = corner.unwrap();
        assert_eq!(
            HashSet::<_>::from_iter(corner),
            HashSet::from_iter([
                (FaceId::Up, 2, 2),
                (FaceId::Front, 0, 2),
                (FaceId::Right, 0, 0)
            ])
        );

        cube.apply_step(Step::new(Movement::Rotation(FaceId::Right), 1));

        let corner = cube.find_corner([FaceId::Up, FaceId::Right, FaceId::Front]);
        assert!(corner.is_some());
        let corner = corner.unwrap();
        assert_eq!(
            HashSet::<_>::from_iter(corner),
            HashSet::from_iter([
                (FaceId::Up, 0, 2),
                (FaceId::Back, 0, 0),
                (FaceId::Right, 0, 2)
            ])
        );
    }

    #[test]
    fn find_edge() {
        let cube = Cube::solved();

        let edge = cube.find_edge([FaceId::Up, FaceId::Right]);
        assert!(edge.is_some());
        let edge = edge.unwrap();
        assert_eq!(
            HashSet::<_>::from_iter(edge),
            HashSet::from_iter([(FaceId::Up, 1, 2), (FaceId::Right, 0, 1)])
        );

        cube.apply_step(Step::new(Movement::Rotation(FaceId::Right), 1));

        let edge = cube.find_edge([FaceId::Up, FaceId::Right]);
        assert!(edge.is_some());
        let edge = edge.unwrap();
        assert_eq!(
            HashSet::<_>::from_iter(edge),
            HashSet::from_iter([(FaceId::Back, 1, 0), (FaceId::Right, 1, 2)])
        );
    }
}
