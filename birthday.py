import numpy as np 
import pandas as pd
import random as rd
import matplotlib.pyplot as plt

#Here we analyze the "birthday paradox" which states that if you have 23 people in a remove, there
#is a ~50% chance that at least two of them have the same birthday. This seems highly counterintuitive
#but it works out in the following way:
    #for one person, the probability of having a unique birthday is simply 1
    #with two people, the probability of unique birthdays is 1*(364/365) since you need the second person
    #to have a different birthday, and there is a 364/365 probability of this. Now with a third person, the same
    #logic follows and the probability of unique birthdays becomes 1*(364/365)*(363/365) and so on
    #then we simply say that the probability of at least two people having the same birthday is 
    #1 minus the probability of unique birthdays.
#We can illustrate with this simple for loop.  
#For all of this analysis, please recall that python indexes from 0, not 1.  Therefore, we set up
#the simulation such that an index of 0 implies 1 person in the room.

x = 1
for i in range(23):
    x = x*((365-i)/365)
    print(i)
print(1-x)    

w = np.full([10000,1],None)
for j in range(0,10000):
    y = rd.choices(range(1,365),k=23)
    w[j] = (len(y) == len(set(y)))

print(1-np.mean(w))    
#Notice that the simulated estimate is quite close to the theoretical value.
#Now let's see how the probability of having multiple people in the room with the same
#birthday changes with the number of people in the room.

z = np.full([100,1],None)
a = np.full([100,1],None)
y=1

for j in range(100):
    y = y*((365-j)/365)
    a[j] = 1-y
    z[j] = j
    
    
hx = [0,22]
hy  = [a[22],a[22]] 
vx = [22,22]
vy = [0,a[22]]
h_x = [0,40]
h_y = [a[40],a[40]]
v_x = [40,40]
v_y = [a[4],a[40]]  
plt.plot(z,a)
plt.plot(hx,hy, color = "green")
plt.plot(vx,vy, color = "green")
plt.plot(h_x,h_y, color = "red")
plt.plot(v_x, v_y, color = "red")
plt.xlabel("Number of People in the Room")
plt.ylabel("Probability of 2+ People with Same Birthday")
#With 23 people in the room the probability of multiple birthdays exceeds 50% as shown
#with the green lines and with only 41 people in the room the probability exceeds 90% as shown
#with the red lines.
    
