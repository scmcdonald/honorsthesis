#this is the library to
import requests
import re

response = requests.get('http://www.usccb.org/about/bishops-and-dioceses/all-dioceses.cfm')

#. means any character
#* 0 to infinite
dioceses=re.findall(r'important;">(.*?)</td>',response.text)
print(len(dioceses))

#FOR LOOP TO PRINT OUT THE RESULTS
for d in dioceses:
    print(d)

#OPEN A FILE, WRITE OUT THE RESULTS
with open ('dioceses.txt','w',encoding='utf8') as my_file:
    #takes each element of the dioceses list and adds a \n which is a line break
    my_file.writelines("\n".join(dioceses))




