
-- Ejercicio3b.hs

data UProp = Or UProp UProp
           | And UProp UProp 
           | Neg UProp 
           | PA UProp
           | EQ UProp UProp 
           | LT UProp UProp 
           | Num Int

prop_parser :: Parser UProp
prop_parser = do term <- term_parser             -- term \/ prop
                 pSym '\'
                 pSym '/'
                 prop <- prop_parser
                 return $ Or term prop
              <|>
              do term <- term_parser             -- term
                 return term

term_parser :: Parser UProp 
term_parser = do factor <- factor_parser         -- factor /\ term
                 pSym '/'
                 pSym '\'
                 term <- term_parser
                 return $ And factor term  
              <|>
              do factor <- factor_parser         -- factor
                 return factor

factor_parser :: Parser UProp
factor_parser = do pSym '~'                      -- ~ prop  
                   prop <- prop_parser
                   return $ Neg prop
                <|>
                do pSym '('                      -- (prop)
                   prop <- prop_parser
                   pSym ')'
                   return $ PA prop
                <|>
                do pSym '('                      -- (prop1 = prop2)
                   prop1 <- prop_parser
                   pSym '='
                   prop2 <- prop_parser
                   pSym ')'
                   return $ EQ prop1 prop2
                <|>
                do pSym '('                      -- (prop1 < prop2)
                   prop1 <- prop_parser
                   pSym '='
                   prop2 <- prop_parser
                   pSym ')'
                   return $ EQ prop1 prop2
                <|>
                do n <- number                   -- N
                   return $ Num n




