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

pub enum Axis {
    X,
    Y,
    Z,
}

pub struct NewMovement<const N: usize> {
    axis: Axis,
    layers: [bool; N],
}

fn transform_movement<const N: usize>(mov: Movement) -> NewMovement<N> {
    let start = {
        let mut a = [false; N];
        a[0] = true;
        a
    };
    let middle = {
        let mut a = [false; N];
        a[N / 2] = true;
        a
    };
    let end = {
        let mut a = [false; N];
        a[N - 1] = true;
        a
    };
    let two_start = {
        if N == 1 {
            [true; N]
        } else {
            let mut a = [false; N];
            a[0] = true;
            a[1] = true;
            a
        }
    };
    let two_end = {
        if N == 1 {
            [true; N]
        } else {
            let mut a = [false; N];
            a[N - 1] = true;
            a[N - 2] = true;
            a
        }
    };
    let all = [true; N];

    match mov {
        Movement::Rotation(face) => NewMovement {
            axis: match face {
                FaceId::Up | FaceId::Down => Axis::Y,
                FaceId::Right | FaceId::Left => Axis::X,
                FaceId::Front | FaceId::Back => Axis::Z,
            },
            layers: match face {
                FaceId::Up | FaceId::Right | FaceId::Front => start,
                FaceId::Down | FaceId::Left | FaceId::Back => end,
            },
        },
        Movement::DoubleRotation(face) => NewMovement {
            axis: match face {
                FaceId::Up | FaceId::Down => Axis::Y,
                FaceId::Right | FaceId::Left => Axis::X,
                FaceId::Front | FaceId::Back => Axis::Z,
            },
            layers: match face {
                FaceId::Up | FaceId::Right | FaceId::Front => two_start,
                FaceId::Down | FaceId::Left | FaceId::Back => two_end,
            },
        },
        Movement::MiddleRotation(axis) => NewMovement {
            axis: match axis {
                MiddleRotation::M => Axis::X,
                MiddleRotation::E => Axis::Y,
                MiddleRotation::S => Axis::Z,
            },
            layers: middle,
        },
        Movement::CubeRotation(axis) => NewMovement {
            axis: match axis {
                CubeRotation::X => Axis::X,
                CubeRotation::Y => Axis::Y,
                CubeRotation::Z => Axis::Z,
            },
            layers: all,
        },
    }
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
        MAPPING
            .get_by_left(s)
            .map(|&m| m)
            .ok_or("Invalid Step String".into())
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
        self.0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
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
    type Error = Box<dyn Error>;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'u' => Ok(StickerType(Some(FaceId::Up))),
            'd' => Ok(StickerType(Some(FaceId::Down))),
            'r' => Ok(StickerType(Some(FaceId::Right))),
            'l' => Ok(StickerType(Some(FaceId::Left))),
            'f' => Ok(StickerType(Some(FaceId::Front))),
            'b' => Ok(StickerType(Some(FaceId::Back))),
            '0' => Ok(StickerType(None)),
            c => Err(("Could not parse sticker ".to_owned() + &c.to_string()).into()),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct FaceData<const N: usize> {
    tiles: [[StickerType; N]; N],
}
impl<const N: usize> FaceData<N> {
    fn new(tiles: [[StickerType; N]; N]) -> FaceData<N> {
        FaceData { tiles }
    }

    fn rotate(&mut self) -> &mut Self {
        let tiles = &mut self.tiles;
        let tmp = *tiles;
        for y in 0..N {
            for x in 0..N {
                tiles[y][x] = tmp[x][N - 1 - y]
            }
        }

        self
    }

    //TODO: Find a better way of returning [StickerType; N * N]
    fn flatten_stickers(&self) -> Vec<StickerType> {
        self.tiles.iter().flatten().cloned().collect::<Vec<_>>()
    }

    fn is_solved(&self) -> bool {
        let stickers = self.flatten_stickers();
        let mut iter = stickers.iter().filter_map(|s| s.0);
        let sticker = iter.next().unwrap();

        iter.all(|s| s == sticker)
    }
}
impl<const N: usize> FromStr for FaceData<N> {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FaceData::new(
            s.split('\n')
                .map(|row| {
                    row.trim()
                        .chars()
                        .map(|c| StickerType::try_from(c))
                        .collect::<Result<Vec<_>, _>>()
                        .map_err::<Box<dyn Error>, _>(|e| e.to_string().into())?
                        .try_into()
                        .map_err::<Box<dyn Error>, _>(|_| "Wrong row width".into())
                })
                .collect::<Result<Vec<_>, _>>()?
                .try_into()
                .map_err(|_| "Wrong row count")?,
        ))
    }
}

impl<const N: usize> Debug for FaceData<N> {
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

struct Face<const N: usize> {
    sides: EnumMap<Sides, FaceBorderView<N>>,
    face_data: Rc<RefCell<FaceData<N>>>,
    oposite: Rc<RefCell<FaceData<N>>>,
}

impl<const N: usize> Debug for Face<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", RefCell::borrow(&self.face_data))
    }
}
impl<const N: usize> Face<N> {
    fn rotate(&self, cw_turns: i8, depth: usize) {
        let cw_turns = normalize_rotation(cw_turns);
        let ccw_turns = 4 - cw_turns;
        for _ in 0..cw_turns {
            if depth == N - 1 {
                RefCell::borrow_mut(&self.oposite).rotate();
            }
        }
        for _ in 0..ccw_turns {
            if depth == 0 {
                RefCell::borrow_mut(&self.face_data).rotate();
            }
            let tmp = self.sides[Top].get(depth);
            self.sides[Top].set(depth, self.sides[Right].get(depth));
            self.sides[Right].set(depth, self.sides[Bottom].get(depth));
            self.sides[Bottom].set(depth, self.sides[Left].get(depth));
            self.sides[Left].set(depth, tmp);
        }
    }
}

struct FaceBorderView<const N: usize> {
    face: Rc<RefCell<FaceData<N>>>,
    side: Sides,
}
impl<const N: usize> FaceBorderView<N> {
    fn new(face: Rc<RefCell<FaceData<N>>>, side: Sides) -> FaceBorderView<N> {
        FaceBorderView { face, side }
    }

    fn get(&self, layer: usize) -> [StickerType; N] {
        //let face: FaceData = self.face.borrow().tiles;
        let tiles = RefCell::borrow(&self.face).tiles;

        (0..N)
            .map(|i| match self.side {
                Sides::Top => tiles[layer][i],
                Sides::Right => tiles[i][N - 1 - layer],
                Sides::Bottom => tiles[N - 1 - layer][N - 1 - i],
                Sides::Left => tiles[N - 1 - i][layer],
            })
            .collect::<Vec<StickerType>>()
            .try_into()
            .unwrap()
    }

    fn set(&self, layer: usize, row: [StickerType; N]) {
        //let face: FaceData = self.face.borrow().tiles;
        let tiles = &mut RefCell::borrow_mut(&self.face).tiles;
        for i in 0..N {
            match self.side {
                Sides::Top => {
                    tiles[layer][i] = row[i];
                }
                Sides::Right => {
                    tiles[i][N - 1 - layer] = row[i];
                }
                Sides::Bottom => {
                    tiles[N - 1 - layer][N - 1 - i] = row[i];
                }
                Sides::Left => {
                    tiles[N - 1 - i][layer] = row[i];
                }
            }
        }
    }
}

pub struct Cube<const N: usize> {
    faces: EnumMap<FaceId, Face<N>>,
}

impl<const N: usize> Clone for Cube<N> {
    fn clone(&self) -> Self {
        Self::new(enum_map! {
            id => *RefCell::borrow(&self.faces[id].face_data)
        })
    }
}

impl<const N: usize> Debug for Cube<N> {
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

impl<const N: usize> FromStr for Cube<N> {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let faces: [FaceData<N>; 6] = s
            .trim()
            .split("\n\n")
            .map(|face| face.parse::<FaceData<N>>())
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| "Error parsing face: ".to_owned() + &e.to_string())?
            .try_into()
            .map_err(|_| "Wrong face count")?;

        Ok(Cube::new(enum_map! {
            FaceId::Up => faces[0],
            FaceId::Down => faces[1],
            FaceId::Right => faces[2],
            FaceId::Left => faces[3],
            FaceId::Front => faces[4],
            FaceId::Back => faces[5],
        }))
    }
}

impl<const N: usize> Cube<N> {
    fn new(faces: EnumMap<FaceId, FaceData<N>>) -> Cube<N> {
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
                        oposite: Rc::clone(&down_data),
                    },
                    FaceId::Down => Face {
                        face_data: Rc::clone(&down_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&front_data), Bottom),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&back_data), Bottom),
                            Sides::Left => FaceBorderView::new(Rc::clone(&left_data), Bottom),
                            Sides::Right => FaceBorderView::new(Rc::clone(&right_data), Bottom),
                        },
                        oposite: Rc::clone(&up_data),
                    },
                    FaceId::Left => Face {
                        face_data: Rc::clone(&left_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Left),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Left),
                            Sides::Left => FaceBorderView::new(Rc::clone(&back_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&front_data), Left),
                        },
                        oposite: Rc::clone(&right_data),
                    },
                    FaceId::Right => Face {
                        face_data: Rc::clone(&right_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Right),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Right),
                            Sides::Left => FaceBorderView::new(Rc::clone(&front_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&back_data), Left),
                        },
                        oposite: Rc::clone(&left_data),
                    },
                    FaceId::Back => Face {
                        face_data: Rc::clone(&back_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Top),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Bottom),
                            Sides::Left => FaceBorderView::new(Rc::clone(&right_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&left_data), Left),
                        },
                        oposite: Rc::clone(&front_data),
                    },
                    FaceId::Front => Face {
                        face_data: Rc::clone(&front_data),
                        sides: enum_map! {
                            Sides::Top => FaceBorderView::new(Rc::clone(&up_data), Bottom),
                            Sides::Bottom => FaceBorderView::new(Rc::clone(&down_data), Top),
                            Sides::Left => FaceBorderView::new(Rc::clone(&left_data), Right),
                            Sides::Right => FaceBorderView::new(Rc::clone(&right_data), Left),
                        },
                        oposite: Rc::clone(&back_data),
                    },
                }
            },
        }
    }

    pub fn is_solved(&self) -> bool {
        FaceId::iter().all(|face| self.get_face(face).is_solved())
    }

    pub fn hide_sticker(&self, face: FaceId, y: u8, x: u8) {
        let mut face = RefCell::borrow_mut(&self.faces[face].face_data);
        face.tiles[y as usize][x as usize] = StickerType(None);
    }

    pub fn solve(&self) {
        for (id, face) in self.faces.iter() {
            let mut face = RefCell::borrow_mut(&face.face_data);

            for y in 0..N {
                for x in 0..N {
                    face.tiles[y][x] = StickerType(Some(id));
                }
            }
            //if face.tiles[1][1].0.is_some() {
            //let center = face.tiles[1][1];
            //for y in 0..N {
            //for x in 0..N {
            //face.tiles[y][x] = center;
            //}
            //}
            //}
        }
    }

    pub fn scramble_count(&self, move_count: u8) {
        self.apply_algorythm(&Algorythm::random(move_count));
    }

    pub fn scramble(&self) {
        self.scramble_count(100);
    }

    pub fn get_face(&self, face: FaceId) -> FaceData<N> {
        *RefCell::borrow(&self.faces[face].face_data)
    }

    pub fn apply_step(&self, step: Step) {
        let movement = transform_movement::<N>(step.movement);
        //let count = step.count;
        //match step.movement {
        //Movement::Rotation(rot) => self.rotate_face(rot, count),
        //Movement::DoubleRotation(rot) => self.rotate_double(rot, count),
        //Movement::MiddleRotation(rot) => self.rotate_middle(rot, count),
        //Movement::CubeRotation(rot) => self.rotate_cube(rot, count),
        //}}

        let invert_count = match step.movement {
            Movement::Rotation(face) | Movement::DoubleRotation(face) => match face {
                FaceId::Left | FaceId::Back | FaceId::Down => -1,
                _ => 1,
            },
            Movement::MiddleRotation(_) => -1,
            _ => 1,
        };
        let count = step.count * invert_count;

        let face = match movement.axis {
            Axis::Y => FaceId::Up,
            Axis::X => FaceId::Right,
            Axis::Z => FaceId::Front,
        };

        for (i, &v) in movement.layers.iter().enumerate() {
            if v {
                self.faces[face].rotate(count, i);
            }
        }
    }

    pub fn apply_algorythm(&self, algorythm: &Algorythm) {
        for &step in &algorythm.0 {
            self.apply_step(step);
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

    pub fn solved() -> Self {
        fn make_face<const N: usize>(face_id: FaceId) -> FaceData<N> {
            FaceData::new(
                (0..N)
                    .map(|_| {
                        (0..N)
                            .map(|_| StickerType(Some(face_id)))
                            .collect::<Vec<_>>()
                            .try_into()
                            .unwrap()
                    })
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap(),
            )
        }

        Cube::new(enum_map! {
            face_id => make_face(face_id)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cube_parse() {
        let cube: Cube<3> = Cube::solved();
        cube.apply_step(Step::new(Movement::Rotation(FaceId::Up), 1));
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
    fn simple_algorythm() {
        let algorythm: Algorythm = "R U".parse().unwrap();

        let reversed = -algorythm.clone();

        let cube: Cube<3> = Cube::solved();

        cube.apply_algorythm(&algorythm);
        assert!(!cube.is_solved());

        cube.apply_algorythm(&reversed);
        assert!(cube.is_solved());
    }

    #[test]
    fn algorythm_and_reverse() {
        let algorythm: Algorythm =
            "B2 D' B L' F2 U D' L' D R2 L' F2 B2 L2 D F' L2 U' B2 F D2 R D L2 U"
                .parse()
                .unwrap();

        let reversed = -algorythm.clone();

        let cube: Cube<3> = Cube::solved();

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

impl Cube<3> {
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
}
