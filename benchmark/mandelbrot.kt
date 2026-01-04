const val BAILOUT = 16
const val MAX_ITERATIONS = 1000

fun mandelbrot(x: Float, y: Float): Int {
    val cr = y - 0.5f
    val ci = x
    var zi = 0.0f
    var zr = 0.0f
    var i = 0

    while (true) {
        i++
        val temp = zr * zi
        val zr2 = zr * zr
        val zi2 = zi * zi
        zr = zr2 - zi2 + cr
        zi = temp + temp + ci
        if (zi2 + zr2 > BAILOUT)
            return i
        if (i > MAX_ITERATIONS)
            return 0
    }
}

fun main() {
    val startTime = System.nanoTime()

    for (y in -39..39) {
        println()
        for (x in -39..39) {
            val result = mandelbrot(x / 40.0f, y / 40.0f)
            print(if (result == 0) "*" else " ")
        }
    }

    val endTime = System.nanoTime()
    val elapsed = (endTime - startTime) / 1_000_000_000.0
    println("\nKotlin Elapsed: %.2f".format(elapsed))
}
