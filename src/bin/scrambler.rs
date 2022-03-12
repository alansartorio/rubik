use rubik::cube::Cube;

fn main() {
    let cube: Cube<3> = Cube::solved();
    cube.scramble();
    println!("{:?}", cube);
}
