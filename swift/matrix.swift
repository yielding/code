#!/usr/bin/env swift

struct Matrix {
  let rows: Int, columns: Int
  var grid: [Double]

  init(rows: Int, columns: Int) {
    self.rows = rows
    self.columns = columns
    self.grid = Array(count: self.rows * self.columns, repeatedValue: 0.0)
  }

  func indexIsValidForRow(row: Int, column: Int) -> Bool {
    return row >= 0 && row < self.rows && column >= 0 && 
    column < self.columns
  }

  subscript(row: Int, column: Int) -> Double {
    get {
      assert(indexIsValidForRow(row, column: column), "Index out of range")
      return grid[(row * self.columns) + column]
    }

    set {
      assert(indexIsValidForRow(row, column: column), "Index out of range")
      grid[(row * self.columns) + column] = newValue 
    }
  }
}

var matrix = Matrix(rows: 2, columns: 2)
matrix[0, 1] = 1.5
matrix[1, 0] = 3.2

print(matrix[1, 0])
