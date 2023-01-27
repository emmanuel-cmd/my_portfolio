print("This program returns the month you were born")

birthday = ("January", "February", "March", "April", "May",
            "June", "July", "August", "September", "October", "November", "December")
birth = input("Type in your birthday in the format DD-MM-YYYY: ")
month = int(birth[3:5]) - 1

print("You were born in", birthday[(month)])


