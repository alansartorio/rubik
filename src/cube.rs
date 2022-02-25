use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    ops::{Index, IndexMut},
    ptr::null,
    rc::{Rc, Weak},
};

use enum_map::{enum_map, Enum, EnumMap};

#[derive(Enum, Default)]
enum Sides {
    #[default]
    Left,
    Right,
    Top,
    Bottom,
}

#[derive(Enum, Default, Clone, Copy)]
enum FaceId {
    #[default]
    Up,
    Down,
    Right,
    Left,
    Front,
    Back,
}

#[derive(Default)]
struct Face {
    sides: EnumMap<Sides, FaceBorderView>,
    tiles: [[FaceId; 3]; 3],
}

#[derive(Default)]
struct FaceBorderView {
    face: Weak<Face>,
    side: Sides,
}
impl FaceBorderView {
    fn new(face: Weak<Face>, side: Sides) -> FaceBorderView {
        FaceBorderView { face, side }
    }
}
impl Index<u8> for FaceBorderView {
    type Output = FaceId;
    fn index(&self, index: u8) -> &Self::Output {
        let face: &Face = self.face.upgrade().unwrap().borrow();
        let tiles = &face.tiles;
        match self.side {
            Sides::Top => [&tiles[0][0], &tiles[0][1], &tiles[0][2]][index as usize],
            Sides::Right => [&tiles[0][2], &tiles[1][2], &tiles[2][2]][index as usize],
            Sides::Bottom => [&tiles[2][2], &tiles[2][1], &tiles[2][0]][index as usize],
            Sides::Right => [&tiles[2][0], &tiles[1][0], &tiles[0][0]][index as usize],
            _ => unreachable!(),
        }
    }
}
impl IndexMut<u8> for FaceBorderView {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        let face: &Face = self.face.upgrade().unwrap().borrow_mut();
        let tiles = &face.tiles;
        match self.side {
            Sides::Top => &mut [&tiles[0][0], &tiles[0][1], &tiles[0][2]][index as usize],
            Sides::Right => &mut [&tiles[0][2], &tiles[1][2], &tiles[2][2]][index as usize],
            Sides::Bottom => &mut [&tiles[2][2], &tiles[2][1], &tiles[2][0]][index as usize],
            Sides::Right => &mut [&tiles[2][0], &tiles[1][0], &tiles[0][0]][index as usize],
            _ => unreachable!(),
        }
    }
}

struct Cube {
    faces: EnumMap<FaceId, Rc<Face>>,
}

impl Cube {
    fn new(faces: EnumMap<FaceId, [[FaceId; 3]; 3]>) -> Cube {
        Cube {
            faces: {
                let mut left = Rc::new(Face {
                    tiles: faces[FaceId::Up],
                    sides: Default::default(),
                });
                let mut right = Rc::new(Face {
                    tiles: faces[FaceId::Right],
                    sides: Default::default(),
                });
                let mut up = Rc::new(Face {
                    tiles: faces[FaceId::Up],
                    sides: Default::default(),
                });
                let mut down = Rc::new(Face {
                    tiles: faces[FaceId::Down],
                    sides: Default::default(),
                });
                let mut front = Rc::new(Face {
                    tiles: faces[FaceId::Front],
                    sides: Default::default(),
                });
                let mut back = Rc::new(Face {
                    tiles: faces[FaceId::Back],
                    sides: Default::default(),
                });
                macro_rules! sides {
                    ($topFace: ident, $top: ident, $bottomFace: ident, $bottom: ident, $rightFace: ident, $right: ident, $leftFace: ident, $left: ident) => {
                        {
                            enum_map! {
                                Sides::Top => FaceBorderView::new(Rc::downgrade(&$topFace), $top),
                                Sides::Bottom => FaceBorderView::new(Rc::downgrade(&$bottomFace), $bottom),
                                Sides::Right => FaceBorderView::new(Rc::downgrade(&$rightFace), $right),
                                Sides::Left => FaceBorderView::new(Rc::downgrade(&$leftFace), $left),
                            }
                        }
                    };
                }
                use Sides::*;
                let up2 = up.borrow_mut();
                up2.sides = sides!(back, Top, front, Top, right, Top, left, Top);
                let down2 = down.borrow_mut();
                down2.sides = sides!(front, Top, back, Top, right, Top, left, Top);
                let left2 = left.borrow_mut();
                left2.sides = sides!(up, Top, down, Top, front, Top, back, Top);
                let right2 = left.borrow_mut();
                right2.sides = sides!(up, Top, down, Top, back, Top, front, Top);
                let front2 = front.borrow_mut();
                front2.sides = sides!(up, Top, down, Top, right, Top, left, Top);
                let back2 = back.borrow_mut();
                back2.sides = sides!(up, Top, down, Top, left, Top, right, Top);

                enum_map! {
                    FaceId::Up => up,
                    FaceId::Down => down,
                    FaceId::Left => left,
                    FaceId::Right => right,
                    FaceId::Back => back,
                    FaceId::Front => front,
                }
            },
        }
    }
}
