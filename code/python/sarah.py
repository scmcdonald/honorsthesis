from selenium import webdriver

import time
driver = webdriver.Chrome()

USERNAMER=""
PASSWORD=""


#library database page
URL='http://marymount.libguides.com/DB_Subjects/N'
driver.get(URL)
time.sleep(2)
link= driver.find_elements_by_partial_link_text("Nexis Uni")[0]
#change link to self so it is not in a new tab
driver.execute_script("arguments[0].setAttribute('target','_self')", link)
link.click()

time.sleep(2)
driver.find_element_by_name("j_username").send_keys(USERNAMER)
driver.find_element_by_name("j_password").send_keys(PASSWORD)
driver.find_element_by_name("_eventId_proceed").click()

time.sleep(2)
#now on search page

#URL Search

URL='https://advance-lexis-com.proxymu.wrlc.org/search/?pdsearchterms="sex"%2C+"abuse"%2C+"Arlington+Dioceses"'
driver.get(URL)

time.sleep(2)
driver.find_element_by_id("podfiltersbuttonpublicationtype").click()
#



time.sleep(2)
pub=driver.find_element_by_xpath("//span[contains(text(),'Newswires & Press Releases')]")
#pub=driver.find_element_by_id("_publicationtype_pf50")
driver.execute_script("window.scrollTo(0, %d)" % pub.location['y'])
time.sleep(1)
driver.execute_script("arguments[0].click();", pub)

time.sleep(5)

timel=driver.find_element_by_xpath("//button[contains(text(),'Timeline')]")
timel.click()
time.sleep(2)
min_data=driver.find_element_by_xpath('//*[@id="refine"]/div[2]/div[4]/div[1]/input')
min_data.clear()
from selenium.webdriver.common.keys import Keys
min_data.send_keys(Keys.CONTROL+"a");
min_data.send_keys(Keys.BACK_SPACE);
min_data.send_keys("01/01/2019")

time.sleep(1)

changetime=driver.find_element_by_xpath('//*[@id="refine"]/div[2]/div[4]/button')

changetime.click()
time.sleep(2)
results=driver.find_element_by_xpath('//*[@id="content"]/header/h2/span')

import re
#print(results.text)
print(re.search(r'\((.*?)\)',results.text).group(1))





#pub.click()
