#### This program calculates the area and circumfrence
#### of a circle based on radius value supplied by the user
import math

print("This program prints the area and circumfrence of a circle")
rad = float(input("Type the radius of the circle:" ))
area = math.pi * (rad**2)
circum = 2 * math.pi * rad

print("The area of the circle is", round(area, 2))
print("The circumference of the circle is", round(circum, 2))
