# Wrapping test to get teamcity messages ...
def tctest(testfun,testname):    
    print ("\n##teamcity[testStarted name='%s']\n"%testname) 
    try:
        testfun()
        print ("##teamcity[testFinished name='%s' message='Test passed']\n"%testname) 
    except:
        print ("##teamcity[testFailed name='%s' message='Test failed']\n"%testname) 
        print ("##teamcity[testFinished name='%s' message='Test failed']\n"%testname) 
#    finally:
#        print ("[END TEST]\n\n")
    


