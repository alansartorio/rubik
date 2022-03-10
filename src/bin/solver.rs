
use rubik::{cube::{Cube, Algorythm}, solver::solve};


fn main() {
    let cube = Cube::solved();
    let scramble = Algorythm::random(4);
    println!("{}", scramble.to_string());

    cube.apply_algorythm(&scramble);

    let solution = solve(&cube);

    println!("{}", solution.to_string());
}
