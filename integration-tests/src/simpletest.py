import random
import time
import string

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

# ================================================================================== #
#                                  FUNCTIONS                                         #
# ================================================================================== #

def randomString(stringLength=4):
    letters = string.ascii_lowercase
    return ''.join(random.choice(letters) for i in range(stringLength))

def register(url, account, password, headless):
    options = webdriver.FirefoxOptions()

    if headless:
        options.add_argument('-headless')

    driver = webdriver.Firefox(options=options)
    driver.implicitly_wait(10)

    try:
        driver.get(url) # Open login url
        driver.find_element(By.XPATH, "//a[text() = 'Create an Account']").click() # Click on authorise button
        driver.find_element(By.XPATH, "(//input)[1]").send_keys(account) # Enter account name
        driver.find_element(By.XPATH, "(//input)[2]").send_keys(password) # Enter password
        driver.find_element(By.XPATH, "//button[text() = 'Create account']").click() # Click on create account button

        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "']").click()
    except:
        driver.quit() # Quit driver
        exit(1) # something went wrong
    else:
        driver.quit() # Quit driver
        exit(0) # everything went right

# ================================================================================== #
#                                    MAIN                                            #
# ================================================================================== #
account = randomString(8)
password = randomString(8)

url = "http://localhost:1234"
headless = True # set as true to run the driver in the background)

print("account: " + account)
print("password: " + password)


register(url, account, password, headless)