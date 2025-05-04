import subprocess
import os

# ---------------------------------------------------- #

def ExecBasicSucces1():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-f", "xml"], capture_output=True, text=True)
    if (result.returncode == 0):
        print("Success to execute (1)")
        return True
    print("Failure to execute (1)")
    return False

def ExecBasicSucces2():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-f", "markdown"], capture_output=True, text=True)
    if (result.returncode == 0):
        print("Success to execute (2)")
        return True
    print("Failure to execute (2)")
    return False

def ExecBasicSucces3():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.md"), "-f", "xml"], capture_output=True, text=True)
    if (result.returncode == 0):
        print("Success to execute (3)")
        return True
    print("Failure to execute (3)")
    return False

def ExecBasicSucces4():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.md"), "-e", "markdown", "-f", "xml"], capture_output=True, text=True)
    if (result.returncode == 0):
        print("Success to execute (4)")
        return True
    print("Failure to execute (4)")
    return False

def ExecBasicSucces5():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-e", "json", "-f", "markdown"], capture_output=True, text=True)
    if (result.returncode == 0):
        print("Success to execute (5)")
        return True
    print("Failure to execute (5)")
    return False

def ExecBasicSucces6():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-e", "json", "-f", "markdown", "-o", (os.getcwd() + "/result.md")], capture_output=True, text=True)
    if (result.returncode == 0):
        print("Success to execute (6)")
        return True
    print("Failure to execute (6)")
    return False

def ExecBasicSucces7():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-f", "xml", "-o", (os.getcwd() + "/result.xml")], capture_output=True, text=True)
    if (result.returncode == 0 and os.path.isfile(os.getcwd() + "/result.xml")):
        os.remove(os.getcwd() + "/result.xml")
        print("Success to execute (7)")
        return True
    print("Failure to execute (7)")
    return False

# ---------------------------------------------------- #

def ExecBasicFailure1():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-z"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (1)")
        return True
    print("Failure to exit (1)")
    return False

def ExecBasicFailure2():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", "/dev/urandom", "-f", "xml"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (2)")
        return True
    print("Failure to exit (2)")
    return False

def ExecBasicFailure3():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-f"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (3)")
        return True
    print("Failure to exit (3)")
    return False

def ExecBasicFailure4():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-f", "yaml"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (4)")
        return True
    print("Failure to exit (4)")
    return False

def ExecBasicFailure5():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.json"), "-f", "-f", "-i", "-o"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (5)")
        return True
    print("Failure to exit (5)")
    return False

def ExecBasicFailure6():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.md"), "-e", "json", "-f", "xml"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (6)")
        return True
    print("Failure to exit (6)")
    return False

def ExecBasicFailure7():
    result = subprocess.run([(os.getcwd() + "/mypandoc"), "-i", (os.getcwd() + "/exemple/exemple.md"), "-f", "xml", "-o"], capture_output=True, text=True)
    if (result.returncode == 84):
        print("Success to exit (7)")
        return True
    print("Failure to exit (7)")
    return False

# ---------------------------------------------------- #

total = 0
counter = 0

total += 1
if (ExecBasicSucces1() == True):
    counter += 1
total += 1
if (ExecBasicSucces2() == True):
    counter += 1
total += 1
if (ExecBasicSucces3() == True):
    counter += 1
total += 1
if (ExecBasicSucces4() == True):
    counter += 1
total += 1
if (ExecBasicSucces5() == True):
    counter += 1
total += 1
if (ExecBasicSucces6() == True):
    counter += 1
total += 1
if (ExecBasicSucces7() == True):
    counter += 1
total += 1
if (ExecBasicFailure1() == True):
    counter += 1
total += 1
if (ExecBasicFailure2() == True):
    counter += 1
total += 1
if (ExecBasicFailure3() == True):
    counter += 1
total += 1
if (ExecBasicFailure4() == True):
    counter += 1
total += 1
if (ExecBasicFailure5() == True):
    counter += 1
total += 1
if (ExecBasicFailure6() == True):
    counter += 1
total += 1
if (ExecBasicFailure7() == True):
    counter += 1

print("Number of tests successful: " + str(counter) + " / " + str(total))
