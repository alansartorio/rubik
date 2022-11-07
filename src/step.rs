use bimap::BiMap;
use enum_map::Enum;
use rand::distributions::uniform::SampleRange;
use rand::seq::SliceRandom;
use std::error::Error;
use std::ops::Neg;
use std::str::FromStr;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

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

#[derive(Clone, Copy, Enum, EnumIter)]
pub enum Axis {
    X,
    Y,
    Z,
}

#[derive(Clone, Copy)]
pub struct NewMovement<const N: usize> {
    pub axis: Axis,
    pub layers: [bool; N],
}

impl<const N: usize> From<Movement> for NewMovement<N> {
    fn from(movement: Movement) -> Self {
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

        match movement {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NotationStep {
    pub movement: Movement,
    pub count: i8,
}

impl Neg for NotationStep {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            movement: self.movement,
            count: -self.count,
        }
    }
}

lazy_static! {
    static ref MAPPING: BiMap<&'static str, NotationStep> = {
        let mut m = BiMap::new();
        m.insert("R", NotationStep::new(Movement::Rotation(FaceId::Right), 1));
        m.insert(
            "R2",
            NotationStep::new(Movement::Rotation(FaceId::Right), 2),
        );
        m.insert(
            "R'",
            NotationStep::new(Movement::Rotation(FaceId::Right), -1),
        );
        m.insert("L", NotationStep::new(Movement::Rotation(FaceId::Left), 1));
        m.insert("L2", NotationStep::new(Movement::Rotation(FaceId::Left), 2));
        m.insert(
            "L'",
            NotationStep::new(Movement::Rotation(FaceId::Left), -1),
        );
        m.insert("U", NotationStep::new(Movement::Rotation(FaceId::Up), 1));
        m.insert("U2", NotationStep::new(Movement::Rotation(FaceId::Up), 2));
        m.insert("U'", NotationStep::new(Movement::Rotation(FaceId::Up), -1));
        m.insert("D", NotationStep::new(Movement::Rotation(FaceId::Down), 1));
        m.insert("D2", NotationStep::new(Movement::Rotation(FaceId::Down), 2));
        m.insert(
            "D'",
            NotationStep::new(Movement::Rotation(FaceId::Down), -1),
        );
        m.insert("F", NotationStep::new(Movement::Rotation(FaceId::Front), 1));
        m.insert(
            "F2",
            NotationStep::new(Movement::Rotation(FaceId::Front), 2),
        );
        m.insert(
            "F'",
            NotationStep::new(Movement::Rotation(FaceId::Front), -1),
        );
        m.insert("B", NotationStep::new(Movement::Rotation(FaceId::Back), 1));
        m.insert("B2", NotationStep::new(Movement::Rotation(FaceId::Back), 2));
        m.insert(
            "B'",
            NotationStep::new(Movement::Rotation(FaceId::Back), -1),
        );
        m.insert(
            "r",
            NotationStep::new(Movement::DoubleRotation(FaceId::Right), 1),
        );
        m.insert(
            "r'",
            NotationStep::new(Movement::DoubleRotation(FaceId::Right), -1),
        );
        m.insert(
            "l",
            NotationStep::new(Movement::DoubleRotation(FaceId::Left), 1),
        );
        m.insert(
            "l'",
            NotationStep::new(Movement::DoubleRotation(FaceId::Left), -1),
        );
        m.insert(
            "u",
            NotationStep::new(Movement::DoubleRotation(FaceId::Up), 1),
        );
        m.insert(
            "u'",
            NotationStep::new(Movement::DoubleRotation(FaceId::Up), -1),
        );
        m.insert(
            "d",
            NotationStep::new(Movement::DoubleRotation(FaceId::Down), 1),
        );
        m.insert(
            "d'",
            NotationStep::new(Movement::DoubleRotation(FaceId::Down), -1),
        );
        m.insert(
            "f",
            NotationStep::new(Movement::DoubleRotation(FaceId::Front), 1),
        );
        m.insert(
            "f'",
            NotationStep::new(Movement::DoubleRotation(FaceId::Front), -1),
        );
        m.insert(
            "b",
            NotationStep::new(Movement::DoubleRotation(FaceId::Back), 1),
        );
        m.insert(
            "b'",
            NotationStep::new(Movement::DoubleRotation(FaceId::Back), -1),
        );
        m.insert(
            "M",
            NotationStep::new(Movement::MiddleRotation(MiddleRotation::M), 1),
        );
        m.insert(
            "M'",
            NotationStep::new(Movement::MiddleRotation(MiddleRotation::M), -1),
        );
        m.insert(
            "E",
            NotationStep::new(Movement::MiddleRotation(MiddleRotation::E), 1),
        );
        m.insert(
            "E'",
            NotationStep::new(Movement::MiddleRotation(MiddleRotation::E), -1),
        );
        m.insert(
            "S",
            NotationStep::new(Movement::MiddleRotation(MiddleRotation::S), 1),
        );
        m.insert(
            "S'",
            NotationStep::new(Movement::MiddleRotation(MiddleRotation::S), -1),
        );
        m.insert(
            "x",
            NotationStep::new(Movement::CubeRotation(CubeRotation::X), 1),
        );
        m.insert(
            "x'",
            NotationStep::new(Movement::CubeRotation(CubeRotation::X), -1),
        );
        m.insert(
            "y",
            NotationStep::new(Movement::CubeRotation(CubeRotation::Y), 1),
        );
        m.insert(
            "y'",
            NotationStep::new(Movement::CubeRotation(CubeRotation::Y), -1),
        );
        m.insert(
            "z",
            NotationStep::new(Movement::CubeRotation(CubeRotation::Z), 1),
        );
        m.insert(
            "z'",
            NotationStep::new(Movement::CubeRotation(CubeRotation::Z), -1),
        );
        m
    };
}

impl FromStr for NotationStep {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        MAPPING
            .get_by_left(s)
            .copied()
            .ok_or_else(|| "Invalid Step String".into())
    }
}

impl ToString for NotationStep {
    fn to_string(&self) -> String {
        (*MAPPING.get_by_right(self).unwrap()).to_owned()
    }
}

impl NotationStep {
    pub fn new(movement: Movement, count: i8) -> Self {
        Self { movement, count }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NotationAlgorythm(pub Vec<NotationStep>);

impl ToString for NotationAlgorythm {
    fn to_string(&self) -> String {
        self.0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" ")
    }
}

impl<const N: usize> From<NotationStep> for Step<N> {
    fn from(other: NotationStep) -> Self {
        let invert_count = match other.movement {
            Movement::Rotation(face) | Movement::DoubleRotation(face) => match face {
                FaceId::Left | FaceId::Back | FaceId::Down => -1,
                _ => 1,
            },
            Movement::MiddleRotation(_) => -1,
            _ => 1,
        };
        let count = other.count * invert_count;
        Step::<N> {
            movement: other.movement.into(),
            count,
        }
    }
}

impl<const N: usize> From<NotationAlgorythm> for Algorythm<N> {
    fn from(other: NotationAlgorythm) -> Self {
        Algorythm(other.0.iter().cloned().map(Into::into).collect())
    }
}

impl<const N: usize> Algorythm<N> {
    pub fn random(depth: u8) -> Algorythm<N> {
        let mut steps = vec![];
        let mut rng = rand::thread_rng();
        for _ in 0..depth {
            let axis = *Axis::iter().collect::<Vec<_>>().choose(&mut rng).unwrap();
            let layer = (0..N).sample_single(&mut rng);
            let count: i8 = (-1..=2).sample_single(&mut rng);
            let mut layers = [false; N];
            layers[layer] = true;
            steps.push(Step::<N> {
                movement: NewMovement::<N> { axis, layers },
                count,
            });
        }

        Algorythm(steps)
    }
}

impl Neg for NotationAlgorythm {
    type Output = Self;

    fn neg(self) -> Self::Output {
        NotationAlgorythm(self.0.iter().rev().cloned().map(Neg::neg).collect())
    }
}

impl FromStr for NotationAlgorythm {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split(' ')
            .filter(|s| !s.is_empty())
            .map(str::parse::<NotationStep>)
            .collect::<Result<Vec<NotationStep>, Self::Err>>()
            .map(NotationAlgorythm)
    }
}

#[derive(Clone, Copy)]
pub struct Step<const N: usize> {
    pub movement: NewMovement<N>,
    pub count: i8,
}

impl<const N: usize> Neg for Step<N> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            movement: self.movement,
            count: -self.count,
        }
    }
}

#[derive(Clone)]
pub struct Algorythm<const N: usize>(pub Vec<Step<N>>);

impl<const N: usize> Neg for Algorythm<N> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Algorythm(self.0.iter().rev().cloned().map(Neg::neg).collect())
    }
}
