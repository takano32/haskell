module BMI where
type BMI = Double
stdBMI :: BMI
stdBMI = 22.0

type Height = Double
type Weight = Double
bmi :: (Height, Weight) -> BMI
bmi = undefined

