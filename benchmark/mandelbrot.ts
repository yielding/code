const BAILOUT = 16.0;
const MAX_ITERATIONS = 1000;

function mandelbrot(x: number, y: number): number {
  const cr = y - 0.5;
  const ci = x;
  let zi = 0.0;
  let zr = 0.0;
  let i = 0;

  while (true) {
    i++;
    const temp = zr * zi;
    const zr2 = zr * zr;
    const zi2 = zi * zi;
    zr = zr2 - zi2 + cr;
    zi = temp + temp + ci;

    if (zi2 + zr2 > BAILOUT) {
      return i;
    }

    if (i > MAX_ITERATIONS) {
      return 0;
    }
  }
}

function main(): void {
  const start = performance.now();

  for (let y = -39; y <= 39; y++) {
    let line = "";
    for (let x = -39; x <= 39; x++) {
      const result = mandelbrot(x / 40.0, y / 40.0);
      line += result === 0 ? "*" : " ";
    }
    console.log(line);
  }

  const elapsed = (performance.now() - start) / 1000;
  console.log(`TypeScript Elapsed: ${elapsed.toFixed(2)}`);
}

main();
