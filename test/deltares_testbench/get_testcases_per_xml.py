import os

with open(r'd:\tmp\Check_Duplicates\premerge-xml.txt', 'r') as f: 
    for line in f:
        xmlfile = line.strip()
        print(f'python Testbench.py --list --config configs/dimr/{xmlfile} --filter "program=dflowfm:testcase=e02" --skip-download all --skip-run > {xmlfile}.txt 2>&1')
        os.system(f'python Testbench.py --list --config configs/dimr/{xmlfile} --filter "program=dflowfm:testcase=e02" --skip-download all --skip-run > {xmlfile}.txt 2>&1')

