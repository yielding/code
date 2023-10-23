data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)


data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin, push :: TurnstileState -> TurnstileOutput

coin _ = Thank

push Unlocked = Open
push Locked   = Tut
