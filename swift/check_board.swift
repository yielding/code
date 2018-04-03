#!/usr/bin/env swift

struct CheckerBoard {
  // 아래 boardColrs는 CheckBoard가 생성될 때 마다 
  // closure {}에 의해 초기화 된다.
  //
  let boardColors: [Bool] = {
    var tempBoard = [Bool]()
    var isBlack = false
    for i in 1...10 {
      for j in 1...10 {
        tempBoard.append(isBlack)
        isBlack = !isBlack
      }
      isBlack = !isBlack
    }

    return tempBoard
  } ()

  func squareIsBlackAtRow(_ row: Int, column: Int) -> Bool {
    return boardColors[(row * 10) + column]
  }
}

let board = CheckerBoard()
print(board.squareIsBlackAtRow(0, column: 1))
print(board.squareIsBlackAtRow(9, column: 9))
