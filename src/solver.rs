use crate::cube::Cube;
use crate::step::{NotationAlgorythm as Algorythm, NotationStep as Step};

fn depth_solve(cube: &Cube<3>, depth: u32) -> Option<Vec<Step>> {
    let valid_steps: Vec<Step> = "R R' L L' U U' F F' D D' B B'"
        .parse::<Algorythm>()
        .unwrap()
        .0;

    if depth == 0 {
        return if cube.is_solved() { Some(vec![]) } else { None };
    }

    for step in valid_steps {
        let copy = cube.clone();
        copy.apply_notation_step(step);
        if let Some(mut algorythm) = depth_solve(&copy, depth - 1) {
            algorythm.insert(0, step);
            return Some(algorythm);
        }
    }

    return None;
}

pub fn solve(cube: &Cube<3>) -> Algorythm {
    Algorythm(
        (0..)
            .find_map(|depth| {
                eprintln!("Searching depth: {}", depth);
                depth_solve(&cube, depth)
            })
            .unwrap(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cube::Cube;
    use crate::step::NotationStep;

    #[test]
    fn test_solve() {
        let cube = Cube::solved();

        assert_eq!(solve(&cube), Algorythm(vec![]));

        cube.apply_step("R".parse::<NotationStep>().unwrap().into());

        assert_eq!(solve(&cube), "R'".parse().unwrap());

        cube.apply_step("R".parse::<NotationStep>().unwrap().into());
        cube.apply_algorythm(&solve(&cube).into());

        assert!(cube.is_solved());
    }
}
