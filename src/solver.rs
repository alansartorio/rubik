use crate::cube::{Algorythm, Cube, Step};
use std::iter;

fn depth_solve(cube: &Cube<3>, depth: u32, algorythms: &Vec<Algorythm>) -> Option<Vec<Step>> {
    //let valid_steps: Vec<Algorythm> =
        //iter::once("R U R' U' R' F R2 U' R' U' R U R' F'".parse().unwrap())
            //.chain(
                //"x y z"
                    //.parse::<Algorythm>()
                    //.unwrap()
                    //.0
                    //.iter()
                    //.map(|&step| Algorythm(vec![step])),
            //)
            //.collect();

    if depth == 0 {
        return if cube.is_solved() { Some(vec![]) } else { None };
    }

    for step in algorythms {
        let copy = cube.clone();
        copy.apply_algorythm(&step);
        if let Some(mut algorythm) = depth_solve(&copy, depth - 1, algorythms) {
            algorythm.splice(0..0, step.0.clone());
            return Some(algorythm);
        }
    }

    return None;
}

pub fn solve(cube: &Cube<3>, algorythms: &Vec<Algorythm>) -> Algorythm {
    Algorythm(
        (0..)
            .find_map(|depth| {
                eprintln!("Searching depth: {}", depth);
                depth_solve(&cube, depth, algorythms)
            })
            .unwrap(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cube::Cube;

    //#[test]
    //fn test_solve() {
    //let cube = Cube::solved();

    //assert_eq!(solve(&cube), Algorythm(vec![]));

    //cube.apply_step("R".parse().unwrap());

    //assert_eq!(solve(&cube), "R'".parse().unwrap());

    //cube.apply_step("R".parse().unwrap());
    //cube.apply_algorythm(&solve(&cube));

    //assert!(cube.is_solved());
    //}
}
