directory = ".\\CodeCoverage"

from os import listdir
from os.path import isfile, join
files = [f for f in listdir(directory) if isfile(join(directory, f))]

modules = []
allRoutines = []

for file in files:
    module = None
    filePath = join(directory, file)

    if '.' not in file:
        continue

    extension = file.split('.')[1].upper()

    if 'HTML' not in extension:
        continue

    print "File:", file
    f = open(filePath, 'r')
    document = f.readlines()
    f.close()
    f = open(filePath, 'w')
    for line in document:
        for filename in files:
            line = line.replace(filename, filename.lower())
        f.write(line)
    f.close()