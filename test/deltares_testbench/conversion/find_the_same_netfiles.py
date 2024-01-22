import os, os.path, filecmp

directory = '.'

allnetfiles = []

for root, dirs, files in os.walk(directory):
    for f in files:
        fullpath = os.path.join(root, f)
        if os.path.splitext(fullpath)[1] == '.nc':
             allnetfiles.append(fullpath)

doubles = []
n = 0

for item1 in allnetfiles:
    if item1 not in doubles:
        found = False
        for item2 in allnetfiles:
            if item1 < item2:
                if filecmp.cmp(item1, item2):
                    print item1 ,  ' and ' , item2 , ' are equal'
                    doubles.append(item2)
                    found = True
                    n = n + 1
        if not found:
            print item1,  ' is only used once'

print 'found ', n, ' double nc files.' 
