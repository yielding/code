#!/usr/bin/env swift

enum Rank: Int {
    case Ace = 1
    case Two, Three, Four, Six, Seven, Eight, Nine, Ten
    case Jack, Queen, King

    func simpleDescription() -> String {
        switch self {
        case .Ace:   return "ace"
        case .Jack:  return "jack"
        case .Queen: return "queen"
        case .King:  return "king"
        default:     return String(self.rawValue)
        }
    }
}

enum Suit {
    case Spades, Hearts, Diamonds, Clubs
    func simpleDescription() -> String {
        switch self {
        case .Spades:   return "spades"
        case .Hearts:   return "hearts"
        case .Diamonds: return "diamonds"
        case .Clubs:    return "clubs"
        }
    }
}

struct Card {
    var rank: Rank
    var suit: Suit
    func simpleDescription() -> String {
        return "The \(rank.simpleDescription()) of" + 
                   "\(suit.simpleDescription())"
    }
}

/*
let ace = Rank.Ace
let aceRawValue = ace.rawValue

print(ace.simpleDescription())
print(aceRawValue)

if let convertedRank = Rank(rawValue: 11) {
    let threeDesc = convertedRank.simpleDescription()
    print(threeDesc)
}

let threeOfSpades = Card(rank: .Three, suit: .Spades)
let threeOfSpadesDesc = threeOfSpades.simpleDescription()

print(threeOfSpadesDesc)
*/

enum ServerResponse {
    case Result(String, String)
    case Error(String)
}

let success = ServerResponse.Result("6:00 am", "8:09 pm")
let failure = ServerResponse.Error("Out of cheese.")

switch success {
case let .Result(sunrise, sunset):
    let serverResponse = "Sunrise is at \(sunrise) and sunset is at \(sunset)"
case let .Error(error):
    let serverResponse = "Failure... \(error)"
}
