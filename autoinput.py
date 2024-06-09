from selenium import webdriver
from selenium.webdriver.support.ui import *
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC

data = input()
data = data.split(',')[1:]

url = 'https://docs.google.com/forms/d/e/1FAIpQLSdUPZOMp-4KVPaxwzQ4kKyuJxqwjMDORSvWXm2WB0aVI44FOw/viewform?usp=sf_link'

driver = webdriver.Chrome()
driver.get(url)

wait = WebDriverWait(driver, 10)

college_input = wait.until(
    EC.element_to_be_clickable((By.XPATH, '(//*[@jscontroller="sWGJ4b"]//input[@jsname="YPqjbf"])[1]'))
)
college_input.send_keys(data[0])

major_input = wait.until(
    EC.element_to_be_clickable((By.XPATH, '(//*[@jscontroller="sWGJ4b"]//input[@jsname="YPqjbf"])[2]'))
)
major_input.send_keys(data[1])

data = list(map(int, data[2:]))

for i, v in enumerate(data,1):
    if v == -1:
        continue
    _input = wait.until(
        EC.element_to_be_clickable((By.XPATH, f'((//*[@jscontroller="wPRNsd"])[{i}]//div[@jscontroller="D8e5bc"])[{v}]'))
    )
    _input.click()
    print(f'{i} {v} clicked!')
    #driver.implicitly_wait(0.2)


input()
# .//*[@jscontroller="sWGJ4b"]/*[@class="Xb9hP"]

"""
sWGJ4b

Xb9hP

vd3tt
"""