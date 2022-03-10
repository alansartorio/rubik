use rubik::cube::Cube;

fn main() {
    let cube = Cube::solved();
    cube.scramble();
    println!("{:?}", cube);
}
