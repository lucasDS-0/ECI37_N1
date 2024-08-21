
-- Ejercicio3b.hs

data UProp = Or UProp UProp
           | And UProp UProp 
           | Neg UProp 
           | PA UProp
           | EQ UProp UProp 
           | LT UProp UProp 
           | Num Int

prop :: Parser UProp
prop_par = do term <- prop_par           -- term \/ prop
              pSmy '\'
              pSym '/'
              prop <- prop_par
              return $ Or term prop
           <|>
           do factor <- prop_par         -- factor /\ term
              pSmy '/'
              pSym '\'
              term <- prop_par
              return $ And factor term  
           <|>
           do pSym '~'                   -- ~ prop  
              prop <- prop_par
              return $ Neg prop
           <|>
           do pSym '('                   -- (prop)
              prop <- prop_par
              pSym ')'
              return $ PA prop
           <|>
           do pSym '('                   -- (prop1 = prop2)
              prop1 <- prop_par
              pSym '='
              prop2 <- prop_par
              pSym ')'
              return $ EQ prop1 prop2
           <|>
           do pSym '('                   -- (prop1 < prop2)
              prop1 <- prop_par
              pSym '='
              prop2 <- prop_par
              pSym ')'
              return $ EQ prop1 prop2
           <|>
           do n <- number                -- N
              return $ Num n
