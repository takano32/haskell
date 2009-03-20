
module BMI where
type BMI = Double
stdBMI :: BMI
stdBMI = 22.0

type Height = Double
type Weight = Double

bmi :: (Height, Weight) -> BMI
-- bmi = undefined
-- bmi (h, w) = "ghoehofhodahsof"

bmi (h, w) = w / h^2

-- bmi171 :: (Weight) -> BMI
-- bmi171 (w) = bmi(1.71, w)
-- bmi171 = \ w -> bmi (1.71)

mkbmi :: Height -> (Weight -> BMI)
mkbmi h = \ w -> bmi(h, w)

bmi171 = mkbmi 1.71
bmi151 = mkbmi 1.51

-- mk bmi = \ h -> (\ w -> bmi (h,w))
-- mk f = \ x -> ( \ y -> f (x, y))
mk f x y = f (x,y)


