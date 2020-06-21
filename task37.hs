type Assoc k v = [(k,v)]

tryFind :: Eq k => Assoc k v -> k -> Maybe v
tryFind [] x = Nothing
tryFind ((z,z') : zs) x
    | z == x     = Just z'
    | otherwise  = tryFind zs x

data Prop = Cst Bool
           | Vr Char
           | Not Prop
           | And Prop Prop
           | Imp Prop Prop
           | Or Prop Prop
           | Eqv Prop Prop
          deriving (Eq,Read)

instance Show Prop where
    show (Cst b) = if b then "T" else "F"
    show (Vr c) = [c]
    show (Not p) = "~" ++ show p
    show (And p q) = "(" ++ show p ++ " /\\ " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " \\/ " ++ show q ++ ")"
    show (Imp p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show (Eqv p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

type PropSubst = Assoc Char Prop

sub :: PropSubst -> Prop -> Prop
sub _ (Cst b) = Cst b
sub s (Not p) = Not (sub s p)
sub s (And p q) = And (sub s p) (sub s q)
sub s (Imp p q) = Imp (sub s p) (sub s q)
sub s (Or p q) = Or (sub s p) (sub s q)
sub s (Eqv p q) = Eqv (sub s p) (sub s q)
sub s (Vr c) = case tryFind s c of
                   Just p -> p
                   Nothing -> Vr c

-- Composition of two substitutions is just applying PropSubst two times.
