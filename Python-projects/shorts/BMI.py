print("This program calculates your body mass index") 

w = float(input("Enter your weight in kg: " ) )
h = float(input("Enter your height in metres: " ) )

bmi = w / (h ** 2)

print("Your BMI is: ", round(bmi,2))

#if (bmi <= 18.5):
 #   print("Underweight")
#elif (18.5 < bmi <= 24.9):
 #   print("Normal weight")
#elif (24.9 < bmi <= 29.9):
 #   print("Overweight")
#else:
 #   print("Obesity")

if (bmi <= 18.5):
    classification = "Underweight"
elif (bmi > 18.5 and bmi <= 24.9):
    classification = "Normal weight"
elif (bmi > 24.9 and bmi <= 29.9):
    classification = "Overweight"
else:
    classification = "Obesity"

print("The classification of your BMI is:", classification) 
