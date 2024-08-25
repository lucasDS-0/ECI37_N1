
-- Ejercicio3b.hs

data UProp = Or UProp UProp
           | And UProp UProp 
           | Neg UProp 
           | PA UProp
           | EQ UProp UProp 
           | LT UProp UProp 
           | Num Int

