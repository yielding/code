use std::time::Instant;

const BAILOUT: f32 = 16.0;
const MAX_ITERATIONS: i32 = 1000;

fn mandelbrot(x: f32, y: f32) -> i32 {
    let cr = y - 0.5;
    let ci = x;
    let mut zi = 0.0f32;
    let mut zr = 0.0f32;
    let mut i = 0;

    loop {
        i += 1;
        let temp = zr * zi;
        let zr2 = zr * zr;
        let zi2 = zi * zi;
        zr = zr2 - zi2 + cr;
        zi = temp + temp + ci;
        if zi2 + zr2 > BAILOUT {
            return i;
        }
        if i > MAX_ITERATIONS {
            return 0;
        }
    }
}

fn main() {
    let start = Instant::now();

    for y in -39..=39 {
        println!();
        for x in -39..=39 {
            let result = mandelbrot(x as f32 / 40.0, y as f32 / 40.0);
            print!("{}", if result == 0 { "*" } else { " " });
        }
    }

    let elapsed = start.elapsed().as_secs_f64();
    println!("\nRust Elapsed: {:.2}", elapsed);
}
