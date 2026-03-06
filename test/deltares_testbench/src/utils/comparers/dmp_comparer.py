from typing import Optional


class DmpComparer:
    @staticmethod
    def path(path) -> None:
        print(path)


def path(fullpath) -> None:
    print(fullpath)


def compareLine(line1, path) -> None:
    pass


def getLines(txt):
    txt1 = txt[1]
    list = []
    for tmp in txt1:
        list.append(tmp)
    return list


def calcDif(param1, param2):
    if param1 == param2:
        return "100.0"
    try:
        param1 = int(param1)
        param2 = int(param2)
    except:
        return "0.0"
    dif = abs(param1 - param2)
    reldif = ((param1 - dif) / param1) * 100.0
    return str(reldif)


def createArray(list):
    list.split(" ")
    for line in list:
        line = line.strip()
    return list


def createTuple(str1, str2, dict1):
    try:
        float(str2)
        while str1 in dict1:
            str1 = str1 + "1"
        return [str1, str2]
    except:
        while str2 in dict1:
            str2 = str2 + "1"
        return [str2, str1]


def compareTxt(txt1, txt2) -> Optional[bool]:
    lines1 = getLines(txt1)
    lines2 = getLines(txt2)
    dict1 = {}
    dict2 = {}

    if lines1.__len__() != lines2.__len__():  # Make sure txtblocks have the same size
        print("Lists have different lengths, aborting...")
        return False
    counter = 0
    for line in lines1:
        if "=" in line:
            # Line contains an '=' token, has to be done more accurate since layout of lines can differ
            # Trims the values before and after the '=' token and adds the tuple to the dictionairy
            split = line.split("=", 1)
            str1 = split[0].strip()
            str2 = split[1].strip()
            tuple = createTuple(str1, str2, dict1)
            dict1[tuple[0]] = tuple[1]

        elif ":" in line:
            # Line contains an '=' token, has to be done more accurate since layout of lines can differ
            # Trims the values before and after the '=' token and adds the tuple to the dictionairy
            split = line.split(":", 1)
            str1 = split[0].strip()
            str2 = split[1].strip()
            tuple = createTuple(str1, str2, dict1)
            dict1[tuple[0]] = tuple[1]

        else:  # Don't forget to add trimming
            split = line.split(" ")
            if split.__len__() < 2:
                print("Found a single text element, skipping..")
            else:
                dict1[counter] = split
                counter += 1

    # Exactly the same as above but for the other dictionairy, this code could possibly be moved to a seperate method
    counter = 0
    for line in lines2:
        if "=" in line:
            split = line.split("=", 1)
            str1 = split[0].strip()
            str2 = split[1].strip()
            tuple = createTuple(str1, str2, dict2)
            dict2[tuple[0]] = tuple[1]

        elif ":" in line:
            split = line.split(":", 1)
            str1 = split[0].strip()
            str2 = split[1].strip()
            tuple = createTuple(str1, str2, dict2)
            dict2[tuple[0]] = tuple[1]

        else:  # Don't forget to add trimming
            split = line.split(" ")
            if split.__len__() < 2:
                print("Found a single text element, skipping..")
            else:
                dict2[counter] = split
                counter += 1

    # print dict1
    print("===== COMPARISON =====")
    for output1 in dict1:
        difference = calcDif(dict1[output1], dict2[output1])
        print(output1)
        print(dict1[output1])
        print(dict2[output1])
        print("\n")
