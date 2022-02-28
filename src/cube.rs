use rand::seq::SliceRandom;
use std::char;
use std::convert::TryFrom;
use std::fmt::{Debug, Display};
use std::str::FromStr;
use std::{cell::RefCell, convert::TryInto, rc::Rc};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use enum_map::{enum_map, Enum, EnumMap};

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

#[derive(Debug, Enum, EnumIter, PartialEq, Eq, Clone, Copy)]
pub enum FaceId {
    Up,
    Down,
    Right,
    Left,
    Front,
    Back,
}

#[derive(Clone, Copy)]
pub enum MiddleRotation {
    MP,
    MN,
    EP,
    EN,
    SP,
    SN,
}

pub enum Rotation {
    XP,
    XN,
    YP,
    YN,
    ZP,
    ZN,
}

impl Display for FaceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let l = match self {
            Self::Up => 'u',
            Self::Down => 'd',
            Self::Right => 'r',
            Self::Left => 'l',
            Self::Front => 'f',
            Self::Back => 'b',
        };
        write!(f, "{}", l)
    }
}
impl TryFrom<char> for FaceId {
    type Error = FaceIdParseError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'u' => Ok(FaceId::Up),
            'd' => Ok(FaceId::Down),
            'r' => Ok(FaceId::Right),
            'l' => Ok(FaceId::Left),
            'f' => Ok(FaceId::Front),
            'b' => Ok(FaceId::Back),
            _ => Err(FaceIdParseError),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct FaceData {
    tiles: [[FaceId; 3]; 3],
}
impl FaceData {
    fn new(tiles: [[FaceId; 3]; 3]) -> FaceData {
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

    fn flatten_stickers(&self) -> [FaceId; 9] {
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
        let mut iter = stickers.iter();
        let sticker = *iter.next().unwrap();

        iter.all(|s| *s == sticker)
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
                        .map(|c| FaceId::try_from(c).unwrap())
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
        let ccw_turns = (4 - cw_turns).rem_euclid(4);
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
    fn get(&self) -> [FaceId; 3] {
        //let face: FaceData = self.face.borrow().tiles;
        let tiles = RefCell::borrow(&self.face).tiles;
        match self.side {
            Sides::Top => [tiles[0][0], tiles[0][1], tiles[0][2]],
            Sides::Right => [tiles[0][2], tiles[1][2], tiles[2][2]],
            Sides::Bottom => [tiles[2][2], tiles[2][1], tiles[2][0]],
            Sides::Left => [tiles[2][0], tiles[1][0], tiles[0][0]],
        }
    }
    fn set(&self, row: [FaceId; 3]) {
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

    pub fn solve(&self) {
        for face in self.faces.values() {
            let mut face = RefCell::borrow_mut(&face.face_data);
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

    pub fn scramble(&self) {
        let moves = [
            FaceId::Up,
            FaceId::Down,
            FaceId::Right,
            FaceId::Left,
            FaceId::Front,
            FaceId::Back,
        ];

        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            let choice = *moves.choose(&mut rng).unwrap();
            self.rotate_face(choice, 1);
        }
    }

    pub fn get_face(&self, face: FaceId) -> FaceData {
        *RefCell::borrow(&self.faces[face].face_data)
    }

    pub fn rotate_face(&self, face: FaceId, cw_turns: i8) {
        self.faces[face].rotate(cw_turns);
    }

    pub fn rotate_double(&self, face: FaceId, cw_turns: i8) {
        let cw_turns = cw_turns.rem_euclid(4);
        self.faces[face].rotate(cw_turns);

        let middle = match face {
            FaceId::Up => MiddleRotation::EN,
            FaceId::Down => MiddleRotation::EP,
            FaceId::Right => MiddleRotation::MN,
            FaceId::Left => MiddleRotation::MP,
            FaceId::Front => MiddleRotation::SN,
            FaceId::Back => MiddleRotation::SP,
        };
        for _ in 0..cw_turns {
            self.rotate_middle(middle);
        }
    }

    pub fn rotate_cube(&self, rotation: Rotation) {
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

        match rotation {
            Rotation::XP => {
                borrow_mut!(FaceId::Right).rotate().rotate().rotate();
                borrow_mut!(FaceId::Left).rotate();
                borrow_mut!(FaceId::Back).rotate().rotate();
                rotate!(FaceId::Up, FaceId::Back, FaceId::Down, FaceId::Front);
                borrow_mut!(FaceId::Back).rotate().rotate();
            }
            Rotation::XN => {
                borrow_mut!(FaceId::Right).rotate();
                borrow_mut!(FaceId::Left).rotate().rotate().rotate();
                borrow_mut!(FaceId::Back).rotate().rotate();
                rotate!(FaceId::Up, FaceId::Front, FaceId::Down, FaceId::Back);
                borrow_mut!(FaceId::Back).rotate().rotate();
            }
            Rotation::YP => {
                borrow_mut!(FaceId::Up).rotate().rotate().rotate();
                borrow_mut!(FaceId::Down).rotate();
                rotate!(FaceId::Front, FaceId::Left, FaceId::Back, FaceId::Right);
            }
            Rotation::YN => {
                borrow_mut!(FaceId::Up).rotate();
                borrow_mut!(FaceId::Down).rotate().rotate().rotate();
                rotate!(FaceId::Front, FaceId::Right, FaceId::Back, FaceId::Left);
            }
            Rotation::ZP => {
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
            Rotation::ZN => {
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
        }
    }

    pub fn rotate_middle(&self, rotation: MiddleRotation) {
        match rotation {
            MiddleRotation::MP => {
                self.rotate_face(FaceId::Left, -1);
                self.rotate_face(FaceId::Right, 1);
                self.rotate_cube(Rotation::XN);
            }
            MiddleRotation::MN => {
                for _ in 0..3 {
                    self.rotate_middle(MiddleRotation::MP);
                }
            }
            MiddleRotation::EP => {
                self.rotate_face(FaceId::Up, 1);
                self.rotate_face(FaceId::Down, -1);
                self.rotate_cube(Rotation::YN);
            }
            MiddleRotation::EN => {
                for _ in 0..3 {
                    self.rotate_middle(MiddleRotation::EP);
                }
            }
            MiddleRotation::SP => {
                self.rotate_face(FaceId::Back, 1);
                self.rotate_face(FaceId::Front, -1);
                self.rotate_cube(Rotation::ZN);
            }
            MiddleRotation::SN => {
                for _ in 0..3 {
                    self.rotate_middle(MiddleRotation::SP);
                }
            }
        };
    }

    pub fn flatten_stickers(&self) -> Vec<FaceId> {
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
}
