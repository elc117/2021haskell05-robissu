--Pratica 5
--Nome: Robson Daniel Marchesan

imcClassif :: Float -> String
imcClassif imc 
        |imc <= 18.5 = "Abaixo"
        |imc <  30 = "Normal"
        |otherwise = "Acima"
            

bmi :: Float -> Float -> String
bmi p h =
      let x = p
          y = h*h
      in  imcClassif (x/y) 

bmi' :: Float -> Float -> String
bmi' peso alt = imcClassif (x/y)
      where x = peso
            y = alt * alt

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
    where digits = take 9 cpf
          dv1 = cpfDV digits [10,9..]
          dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
    let expr = (sum $ zipWith (*) digits mults) `mod` 11
    in (if expr < 2 then 0 else 11-expr)

andTable :: [(Bool, Bool, Bool)]
andTable = [(x,y,x && y) | x <- [True,False], y <- [True,False]]
