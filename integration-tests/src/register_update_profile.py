import random
import time
import string
import traceback

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

# ================================================================================== #
#                                    UTILS                                           #
# ================================================================================== #
def randomString(stringLength=4):
    chars = string.ascii_letters + string.digits
    return ''.join(random.choice(chars) for i in range(stringLength))

# ================================================================================== #
#                                  CONSTANTS                                         #
# ================================================================================== #

url = "http://localhost:1234"

account = randomString(10)
password = randomString(10)
pollTitle = randomString(10)
questionTitle = randomString(10)
questionDetail = randomString(10)

answerTitle = randomString(10)
answerDetail = randomString(10)

headless = True # set as true to run the driver in the background)

# ================================================================================== #
#                                  FUNCTIONS                                         #
# ================================================================================== #

def login():
    options = webdriver.FirefoxOptions()

    if headless:
        options.add_argument('-headless')

    driver = webdriver.Firefox(options=options)
    driver.implicitly_wait(10)

    try:
        # =============== register ============== #
        driver.get(url) # Open login url
        driver.find_element(By.XPATH, "html/body/header/a[text() = 'Create an Account']").click() # Click on authorise button
        driver.find_element(By.XPATH, "(html/body/div/div/div/input)[1]").send_keys(account) # Enter account name
        driver.find_element(By.XPATH, "(html/body/div/div/div/input)[2]").send_keys(password) # Enter password
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Create account']").click() # Click on create account button

        # =============== Update username =============== #
        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "']").click() # Locate the account button and click on it
        driver.find_element(By.XPATH, "html/body/header/div/div/button/h3[text() = 'My Profile']").click() # Click on "My Profile"
        driver.find_element(By.XPATH, "html/body/div/input[1]").send_keys("_updated") # Enter new username
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Update username']").click() # Confirm deletion
        driver.find_element(By.XPATH, "html/body/div/input").send_keys(password) # Enter password
        driver.find_element(By.XPATH, "html/body/div/div/button[text() = 'Confirm']").click() # Confirm deletion

        # =============== Login again =============== #
        driver.find_element(By.XPATH, "html/body/header/a[text() = 'Sign In']").click() # Click on "Sign In" button
        driver.find_element(By.XPATH, "(html/body/div/div/div/input)[1]").send_keys(account + "_updated") # Enter account name
        driver.find_element(By.XPATH, "(html/body/div/div/div/input)[2]").send_keys(password) # Enter password
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Sign-in']").click() # Click on "Sign-in" button

        # =============== Update password =============== #
        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "_updated']").click() # Locate the account button and click on it
        driver.find_element(By.XPATH, "html/body/header/div/div/button/h3[text() = 'My Profile']").click() # Click on "My Profile"
        driver.find_element(By.XPATH, "html/body/div/input[2]").send_keys(password + "_updated") # Enter new username
        driver.find_element(By.XPATH, "html/body/div/input[3]").send_keys(password + "_updated") # Re-enter new username
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Update password']").click() # Confirm deletion
        driver.find_element(By.XPATH, "html/body/div/input").send_keys(password) # Enter passwordW
        driver.find_element(By.XPATH, "html/body/div/div/button[text() = 'Confirm']").click() # Confirm password change

        # =============== Login again =============== #
        driver.find_element(By.XPATH, "html/body/header/a[text() = 'Sign In']").click() # Click on "Sign In" button
        driver.find_element(By.XPATH, "(html/body/div/div/div/input)[1]").send_keys(account + "_updated") # Enter updated account name
        driver.find_element(By.XPATH, "(html/body/div/div/div/input)[2]").send_keys(password + "_updated") # Enter updated password
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Sign-in']").click() # Click on "Sign-in" button
        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "_updated']") # Locate the account button

        # =============== Delete account =============== #
        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "_updated']").click() # Locate the account button and click on it
        driver.find_element(By.XPATH, "html/body/header/div/div/button/h3[text() = 'My Profile']").click() # Click on "My Profile"
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Delete account']").click() # Click on "Delete account"
        driver.find_element(By.XPATH, "html/body/div/input").send_keys(password + "_updated") # Enter password
        driver.find_element(By.XPATH, "html/body/div/div/button[text() = 'Confirm']").click() # Confirm deletion
        
    except Exception:
        driver.quit() # Quit driver
        print(traceback.format_exc())
        exit(1) # something went wrong
    else:
        driver.quit() # Quit driver
        exit(0) # everything went right


# ================================================================================== #
#                                    MAIN                                            #
# ================================================================================== #

print("account :         " + account)
print("password :        " + password)
print("poll title :      " + pollTitle)
print("question title :  " + questionTitle)
print("question detauls: " + questionDetail)
print("answer title :    " + answerTitle)
print("answer details :  " + answerDetail)

login()