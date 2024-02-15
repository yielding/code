function2 a b c = (a * b) + c
function2 a b c = ((*) a b) + c    -- operator sectioning
function2 a b c = (+) ((*) a b) c  -- operator sectioning once more
      = (+) (((*) a) b) c          -- explicit parentheses
      = ((+) . ((*) a)) b c        -- B combinator
      = ((.) (+) ((*) a)) b c      -- operator sectioning 
      = ((.) (+) . (*)) a b c      -- B combinator