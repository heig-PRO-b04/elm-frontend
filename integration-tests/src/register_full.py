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

def register():
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

        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "']") # Locate the account button to ensure the operation was successful

        # =============== Create poll =============== #
        driver.find_element(By.XPATH, "html/body/a/div/div[text() = 'New poll']").click() # Click on "New Poll"
        driver.find_element(By.XPATH, "html/body/div/div/input").send_keys(pollTitle) # Enter poll title
        driver.find_element(By.XPATH, "html/body/div/div/button[text() = 'Confirm']").click() # Click on Confirm

        # =============== Session =============== #
        driver.find_element(By.XPATH, "html/body/div/div/div/p/span[text() = 'Open']").click() # Open the poll

        attr1 = driver.find_element(By.XPATH, "(html/body/div/div/div/p/div/div/div/img)[1]").get_attribute("src") # 1st emoji image
        emoji1 = attr1[len(attr1) - 5] # 1st emoji code
        attr2 = driver.find_element(By.XPATH, "(html/body/div/div/div/p/div/div/div/img)[2]").get_attribute("src") # 2nd emoji image
        emoji2 = attr2[len(attr2) - 5] # 2nd emoji code
        attr3 = driver.find_element(By.XPATH, "(html/body/div/div/div/p/div/div/div/img)[3]").get_attribute("src") # 3rd emoji image
        emoji3 = attr3[len(attr3) - 5] # 3rd emoji code
        attr4 = driver.find_element(By.XPATH, "(html/body/div/div/div/p/div/div/div/img)[4]").get_attribute("src") # 4th emoji image
        emoji4 = attr4[len(attr4) - 5] # 4th emoji code

        print("Code :           "+ emoji1 + emoji2 + emoji3 + emoji4) # print the code

        # =============== Create a question =============== #
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);") # Scroll to bottom of page to prevent "New Question" fab from obscuring elements
        driver.find_element(By.XPATH, "html/body/button/div/div[text() = 'New question']").click() # Click on "New Question"
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td/input").send_keys(questionTitle) # Enter the question title
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td/img").click() # Click on "Set hidden question"
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td/button[text() = 'Create']").click() # Click on "Create"
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td/div/span[text() = '" + questionTitle + "']").click() # Unroll the question
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/div/img[1]").click() # Click on edit question
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/div/input[2]").send_keys(questionDetail) # Enter the question details
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/div/div/button[text() = 'Apply']").click() # Click on apply

        # =============== Create an answer =============== #
        driver.find_element(By.XPATH, "html/body/div/table/tbody/div/div/button[text() = 'Add Answer']").click() # Click on "Add Answer"
        driver.find_element(By.XPATH, "(html/body/div/table/tbody/div/div/input)[1]").send_keys(answerTitle) # Enter answer title
        driver.find_element(By.XPATH, "(html/body/div/table/tbody/div/div/input)[2]").send_keys(answerDetail) # Enter answer detail
        driver.find_element(By.XPATH, "html/body/div/table/tbody/div/div/div/button[text() = 'Create']").click() # Click on "create"

        # =============== Change question attributes =============== #
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/div/div[2]/select/option[text() = '1']").click() # Set Min Answer to 1
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/div/div[3]/select/option[text() = '1']").click() # Set Max Answer to 1, will probably reset min answer to none
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/div/img[3]").click() # Set question to visible

        # =============== Delete the answer =============== #
        driver.find_element(By.XPATH, "html/body/div/table/tbody/div/div/div/div/button[text() = 'Delete']").click() # Delete the answer

        # =============== Delete the question =============== #
        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td/div/button[text() = 'Delete']").click() # Delete the answer
        
        # =============== Return to polls list =============== #
        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "']").click() # Locate the account button and click on it
        driver.find_element(By.XPATH, "html/body/header/div/div/button/h3[text() = 'My Polls']").click() # Click on "My Polls"

        # =============== Delete the poll =============== #
        driver.execute_script("window.scrollTo(0, document.body.scrollHeight);") # Scroll to bottom of page to prevent "New Poll" fab from obscuring elements
        if driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td").text != pollTitle: # First poll isn't the right one
            driver.quit()
            print("First poll isn't the one we just created")
            exit(1)

        driver.find_element(By.XPATH, "html/body/div/table/tbody/tr/td/button[text() = 'Delete']").click() # Delete the first poll

        # =============== Update profile =============== #
        driver.find_element(By.XPATH, "html/body/header/div/button/div[text() = '" + account + "']").click() # Locate the account button and click on it
        driver.find_element(By.XPATH, "html/body/header/div/div/button/h3[text() = 'My Profile']").click() # Click on "My Profile"
        driver.find_element(By.XPATH, "html/body/div/button[text() = 'Delete account']").click() # Click on "Delete account"
        driver.find_element(By.XPATH, "html/body/div/input").send_keys(password) # Enter password
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

register()