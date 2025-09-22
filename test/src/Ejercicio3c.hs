
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
pFail = P $ const []

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

pTermProp :: Parser UProp 
pTermProp = do term <- termParser             -- term \/ prop
               pSym '\\'
               pSym '/'
               prop <- propParser
               return $ Or term prop

pTerm :: Parser UProp
pTerm = do term <- termParser                  -- term
           return term


pFactorTerm :: Parser UProp
pFactorTerm = do factor <- factorParser       -- factor /\ term
                 pSym '/'
                 pSym '\\'
                 term <- termParser
                 return $ And factor term

pFactor :: Parser UProp
pFactor = do factor <- factorParser            -- factor
             return factor

pNegProp :: Parser UProp
pNegProp = do pSym '~'                         -- ~ prop  
              prop <- propParser
              return $ Neg prop

pParenProp :: Parser UProp
pParenProp = do pSym '('                       -- (prop)
                prop <- propParser
                pSym ')'
                return $ PA prop

pEqProp :: Parser UProp
pEqProp = do pSym '('                          -- (prop1 = prop2)
             prop1 <- propParser
             pSym '='
             prop2 <- propParser
             pSym ')'
             return $ Eq prop1 prop2

pLtProp :: Parser UProp
pLtProp = do pSym '('                          -- (prop1 < prop2)
             prop1 <- propParser
             pSym '='
             prop2 <- propParser
             pSym ')'
             return $ Eq prop1 prop2

pNumber :: Parser UProp
pNumber = do n <- number                        -- N
             return $ Num n

propParser :: Parser UProp
propParser = pTermProp
             <|>
             pTerm
              
termParser :: Parser UProp 
termParser = pFactorTerm
             <|>
             pFactor
              
factorParser :: Parser UProp
factorParser = pNegProp 
               <|>
               pParenProp
               <|>
               pEqProp
               <|>
               pLtProp
               <|>
               pNumber