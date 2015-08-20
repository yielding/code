#!/usr/bin/env swift

let N = 1000;
let depth = 200;
let escape2 = 400.0;

func trans_x(x:Int) -> Double
{
	return 3.0 * (Double(x) / Double(N)) - 2.0;
}
func trans_y(y:Int) -> Double
{
	return 3.0 * (Double(y) / Double(N)) - 1.5;
}

func mag2(r:Double, i:Double) -> Double {
	return r*r + i*i
}

func mandel(idx: Int) -> Double {
	let z0_r = trans_x(idx % N)
	let z0_i = trans_y(idx / N)
	
	var z_r = 0.0
	var z_i = 0.0
	
	var k = 0;
	for ; k <= depth && mag2(z_r, z_i) < escape2; ++k {
		let t_r = z_r
		let t_i = z_i
		z_r = t_r * t_r - t_i * t_i + z0_r
		z_i = 2 * t_r * t_i + z0_i
	}
	return log(Double(k) + 1.0 - log(log(max(mag2(z_r, z_i), escape2)) / 2.0) / log(2.0));
}
