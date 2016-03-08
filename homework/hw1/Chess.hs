module Chess where
--
-- | Author:    Kyle Jones
-- | Date:      2016-02-10
-- | Course:    COP-4020

data Color = Black | White deriving (Eq, Show)

data Type = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)

data Position = Position Char Int 

data Piece = Piece Color Type Position

isLegalMove :: Color -> Type -> (Char,Int) -> (Char,Int) -> Bool
isLegalMove c t (f1,r1) (f2,r2) = validPos (Position f1 r1) && validPos (Position f2 r2) && legalMove (Piece c t (Position f1 r1)) (Position f2 r2)

isLegalPosition :: (Char,Int) -> Bool
isLegalPosition (r,c) = validPos $ Position r c

validPos :: Position -> Bool
validPos (Position r c) | r >= 'a' && r <= 'h' &&
                          c >= 1 && c <= 8       = True
                        | otherwise              = False

swapPiece :: Piece -> Type -> Piece
swapPiece (Piece c _ p) t = Piece c t p

legalMove :: Piece -> Position -> Bool
legalMove (Piece _ Pawn (Position _  1))  _                     = False
legalMove (Piece _ Pawn (Position _  8))  _                     = False
legalMove (Piece White Pawn (Position c1 2))  (Position c2 4)   = c1 == c2 
legalMove (Piece White Pawn (Position c1 r1)) (Position c2 r2)  = c1 == c2 && r1 + 1 == r2 
legalMove (Piece Black Pawn (Position c1 7))  (Position c2 5)   = c1 == c2 
legalMove (Piece Black Pawn (Position c1 r1)) (Position c2 r2)  = c1 == c2 && r1 - 1 == r2 
legalMove (Piece _ Bishop (Position c1 r1)) (Position c2 r2)    = abs (fromEnum c2 - fromEnum c1) == abs (r2 - r1)
legalMove (Piece _ Rook (Position c1 r1))   (Position c2 r2)    = (c1 == c2) && (r1 /= r2) || (c1 /= c2) && (r1 == r2)
legalMove (Piece _ King (Position c1 r1))   (Position c2 r2)    = let mc = abs (fromEnum c2 - fromEnum c1) 
                                                                      mr = abs (r2 - r1)
                                                                  in ((mc == 1 || mr == 1) && (mc <= 1 && mr <= 1))
legalMove (Piece _ Knight (Position c1 r1)) (Position c2 r2)    = (abs (fromEnum c2 - fromEnum c1) == 2 && abs (r2 - r1) == 1) ||
                                                                   (abs (fromEnum c2 - fromEnum c1) == 1 && abs (r2 - r1) == 2) 
legalMove queen@(Piece _ Queen _)  pos                          = legalMove (swapPiece queen Bishop) pos || legalMove (swapPiece queen Rook) pos
