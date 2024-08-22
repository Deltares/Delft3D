"""CSV Timeseries comparer.

Copyright (C)  Stichting Deltares, 2024
"""

import os
from datetime import datetime, timedelta
from xml.dom import minidom

import numpy as np
from pandas.plotting import register_matplotlib_converters

import src.utils.comparers.time_series_set_comparer as TimeseriesSetComparer

register_matplotlib_converters()


def getTimeColumnAndFormat(descriptionXML):
    xmltree = minidom.parse(descriptionXML)
    datatree = branch(xmltree)
    general = datatree["timeSeriesImportRun"][0]["import"][0]["general"][0]
    TimeZone = general["importTimeZone"][0]["timeZoneOffset"][0]["#text"]
    table = general["table"][0]
    TimeColumnName = table["dateTimeColumn"][0]["name"]

    #   TODO: read the value column descriptions from the describing XNL as well
    #   for valuecolumns in table['valueColumn']:
    #

    TimeFormat = table["dateTimeColumn"][0]["pattern"]
    TimeFormat = TimeFormat.replace(r"yyyy", "%Y")  # year
    TimeFormat = TimeFormat.replace(r"yyy", "%Y")  # year
    TimeFormat = TimeFormat.replace(r"yy", "%Y")  # year
    TimeFormat = TimeFormat.replace(r"MM", "%M")  # month
    TimeFormat = TimeFormat.replace(r"dd", "%d")  # day
    TimeFormat = TimeFormat.replace(r"HH", "%H")  # hour
    TimeFormat = TimeFormat.replace(r"mm", "%m")  # minute
    TimeFormat = TimeFormat.replace(r"SS", "%S")  # seconds
    return [TimeColumnName, TimeFormat, TimeZone]


def getDatesValues(TSEvent, TSMeta):
    TSDates = []
    TSValues = []
    for event in TSEvent:
        date_str = event["date"]
        time_str = event["time"]
        TSDates.append(datetime.strptime(" ".join([date_str, time_str]), "%Y-%m-%d %H:%M:%S"))
        TSValues.append(float(event["value"]))
    #   return TSDates, TSValues
    return TSDates, np.array(TSValues)


def branch(xmltree):
    newbranch = {}
    for child in xmltree.childNodes:
        tag = child.nodeName
        if tag == "#text":
            newbranch[tag] = xmltree.firstChild.nodeValue
        else:
            if tag not in newbranch:
                newbranch[tag] = []
            newsubbranch = branch(child)
            newbranch[tag].append(newsubbranch)
    if xmltree.attributes is not None:
        for attrib in xmltree.attributes.items():
            newbranch[attrib[0]] = attrib[1]
    return newbranch


def csv2dict(datafile):
    """Convert csv to dictionary.

    Return a dictionary with lists of strings, each list representing a column,
    each key the column name. This is an alternative for a Pandas dataframe.

    Parameters
    ----------
    input
        Input filename.

    Returns
    -------
    dict
        Dictionary holding the data.
    """
    fdata = open(datafile, "r")
    columns = fdata.readline().split(",")
    columns = [s.strip() for s in columns]
    coldata = {}
    for cname in columns:
        if cname > "":
            coldata[cname] = []
    while True:
        line = fdata.readline()
        if not line:
            break
        else:
            entry = line.split(",")
        for icol in range(len(columns)):
            if columns[icol] > "":
                coldata[columns[icol]].append(entry[icol].strip())
    fdata.close()
    return coldata


class CSVTimeseriesComparer(TimeseriesSetComparer.TimeseriesSetComparer):
    """CSV time series comparer.

    Compare two Timeseries files in the comma-separated-value format,
    according to the configuration in file_check.

    Parameters
    ----------
    input
        Left path (reference), right path (compare), file_check

    Returns
    -------
    list
        List of (file_check, parameter, file_check, ResultComparison) tuples.
    """

    def getTimeseriesSet(self, datafile):
        basename = os.path.splitext(datafile)[0]
        descriptionXML = basename + ".xml"
        tname = "time"
        tformat = "%Y-%m-%d %H:%M:%S"
        tzone = 0.0
        if os.path.isfile(descriptionXML):
            tname, tformat, tzone = getTimeColumnAndFormat(descriptionXML)
        basepath = os.path.split(datafile)[0]
        descriptionXML = os.path.join(basepath, "CSVdescriptor.xml")
        if os.path.isfile(descriptionXML):
            tname, tformat, tzone = getTimeColumnAndFormat(descriptionXML)
        df = csv2dict(datafile)
        if tname in df.keys():
            timeslist = []
            for pdtime in df[tname]:
                tshift = timedelta(days=0)
                if "24:" in pdtime:
                    tshift = timedelta(days=1)
                    pdtime = pdtime.replace(" 24:", " 00:")
                timeslist.append(datetime.strptime(pdtime, tformat) + tshift)
        else:
            raise Exception("Time column '%s' not found." % tname)

        TSSet = {}
        for varname in df.keys():
            if varname != tname:
                values = np.array([np.float64(raw) for raw in df[varname]])
                TSSet[varname] = {"None": {"dates": timeslist, "values": values}}  # fake location key 0
        return TSSet
