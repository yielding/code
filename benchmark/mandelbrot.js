var BAILOUT = 16.0;
var MAX_ITERATIONS = 1000;
function mandelbrot(x, y) {
    var cr = y - 0.5;
    var ci = x;
    var zi = 0.0;
    var zr = 0.0;
    var i = 0;
    while (true) {
        i++;
        var temp = zr * zi;
        var zr2 = zr * zr;
        var zi2 = zi * zi;
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
function main() {
    var start = performance.now();
    for (var y = -39; y <= 39; y++) {
        var line = "";
        for (var x = -39; x <= 39; x++) {
            var result = mandelbrot(x / 40.0, y / 40.0);
            line += result === 0 ? "*" : " ";
        }
        console.log(line);
    }
    var elapsed = (performance.now() - start) / 1000;
    console.log("TypeScript Elapsed: ".concat(elapsed.toFixed(2)));
}
main();
