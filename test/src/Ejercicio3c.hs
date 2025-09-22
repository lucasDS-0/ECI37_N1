
import Control.Monad
import GHC.Base hiding ((<|>))

-- | Parser type 

newtype Parser a = P {runP :: String -> [(a,String)]}

instance Functor Parser where
  fmap f p = P $ \cs -> [(f a,cs') | (a,cs') <- runP p cs]

instance Applicative Parser where
  pure a =  P (\cs -> [(a,cs)])
  -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
  (P p) <*> (P q) = P $ \cs -> [ (f a, cs'')  |  (f , cs')   <- p cs
                                              ,  (a , cs'')  <- q cs']

instance Monad Parser where
  return a    = P $ \cs -> [(a,cs)]
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a,cs') <- p cs]

-- | Parsers primitivos

pFail :: Parser a
pFail = P $ \cs -> []

(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
                              []     -> []
                              (x:xs) -> [x]

item :: Parser Char
item = P $ \cs -> case cs of
                    ""     -> []
                    (c:cs) -> [(c,cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do c <- item
            if p c then return c
                   else pFail

pSym :: Char -> Parser Char
pSym c = pSat (== c)

digit :: Parser Int
digit = do c <- pSat isDigit
           return (ord c - ord '0')

isDigit c = (c >= '0') && (c <= '9')

number :: Parser Int
number = do d <- digit
            number' d

number' :: Int -> Parser Int
number' n = do d <- digit
               number' (n*10 + d)
            <|>
            return n

-- Ejercicio3c.hs

data UProp = Or UProp UProp
           | And UProp UProp 
           | Neg UProp 
           | PA UProp
           | Eq UProp UProp 
           | LT UProp UProp 
           | Num Int

p_term_prop :: Parser UProp 
p_term_prop = do term <- term_parser             -- term \/ prop
                 pSym '\\'
                 pSym '/'
                 prop <- prop_parser
                 return $ Or term prop

p_term :: Parser UProp
p_term = do term <- term_parser                  -- term
            return term


p_factor_term :: Parser UProp
p_factor_term = do factor <- factor_parser       -- factor /\ term
                   pSym '/'
                   pSym '\\'
                   term <- term_parser
                   return $ And factor term

p_factor :: Parser UProp
p_factor = do factor <- factor_parser            -- factor
              return factor

p_neg_prop :: Parser UProp
p_neg_prop = do pSym '~'                         -- ~ prop  
                prop <- prop_parser
                return $ Neg prop

p_paren_prop :: Parser UProp
p_paren_prop = do pSym '('                       -- (prop)
                  prop <- prop_parser
                  pSym ')'
                  return $ PA prop

p_eq_prop :: Parser UProp
p_eq_prop = do pSym '('                          -- (prop1 = prop2)
               prop1 <- prop_parser
               pSym '='
               prop2 <- prop_parser
               pSym ')'
               return $ Eq prop1 prop2

p_lt_prop :: Parser UProp
p_lt_prop = do pSym '('                          -- (prop1 < prop2)
               prop1 <- prop_parser
               pSym '='
               prop2 <- prop_parser
               pSym ')'
               return $ Eq prop1 prop2

p_number :: Parser UProp
p_number = do n <- number                        -- N
              return $ Num n

prop_parser :: Parser UProp
prop_parser = p_term_prop
              <|>
              p_term
              
term_parser :: Parser UProp 
term_parser = p_factor_term
              <|>
              p_factor
              
factor_parser :: Parser UProp
factor_parser = p_neg_prop
                <|>
                p_paren_prop
                <|>
                p_eq_prop
                <|>
                p_lt_prop
                <|>
                p_number




