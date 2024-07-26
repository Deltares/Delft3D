# Wrapping test to get teamcity messages ...
def tctest(testfun, testname) -> None:
    print(f"\n##teamcity[testStarted name='{testname}']\n")
    try:
        testfun()
        print(f"##teamcity[testFinished name='{testname}' message='Test passed']\n")
    except:
        print(f"##teamcity[testFailed name='{testname}' message='Test failed']\n")
        print(f"##teamcity[testFinished name='{testname}' message='Test failed']\n")


#    finally:
#        print ("[END TEST]\n\n")
