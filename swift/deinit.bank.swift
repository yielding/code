#!/usr/bin/env swift

class Bank {
    static var coinsInBank = 10_100
    static func vendCoins(var noOfCoinsToVend: Int) -> Int {
        noOfCoinsToVend = min(noOfCoinsToVend, coinsInBank)
        coinsInBank -= noOfCoinsToVend
        return noOfCoinsToVend
    }

    static func receiveCoins(coins: Int) {
        coinsInBank += coins
    }
}

class Player {
    var coinsInPurse: Int
    init (coins: Int) {
        coinsInPurse = Bank.vendCoins(coins)
    }

    func winCoins(coins: Int) {
        coinsInPurse += Bank.vendCoins(coins)
    }

    deinit {
        Bank.receiveCoins(coinsInPurse)
    }
}

var playerOne: Player? = Player(coins: 100)
print("A new player has joined the game with \(playerOne!.coinsInPurse) coins")

print("There are now \(Bank.coinsInBank) coins left in the bank")

playerOne!.winCoins(2_000)
print("There are now \(Bank.coinsInBank) coins left in the bank")
