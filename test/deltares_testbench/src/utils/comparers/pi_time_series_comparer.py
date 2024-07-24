"""FEWS PI Timeseries comparer.

Copyright (C)  Stichting Deltares, 2024
"""

from datetime import datetime
from xml.dom import minidom

import numpy as np

import src.utils.comparers.time_series_set_comparer as TimeseriesSetComparer


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


def getPiTimeSeriesMeta(TSHeader):
    TSMeta = {}
    date_str = TSHeader[0]["pi:startDate"][0]["date"]
    time_str = TSHeader[0]["pi:startDate"][0]["time"]
    TSMeta["startDate"] = datetime.strptime(" ".join([date_str, time_str]), "%Y-%m-%d %H:%M:%S")
    date_str = TSHeader[0]["pi:endDate"][0]["date"]
    time_str = TSHeader[0]["pi:endDate"][0]["time"]
    TSMeta["endDate"] = datetime.strptime(" ".join([date_str, time_str]), "%Y-%m-%d %H:%M:%S")
    TSMeta["units"] = TSHeader[0]["pi:units"][0]["#text"]
    TSMeta["stationName"] = TSHeader[0]["pi:stationName"][0]["#text"]
    TSMeta["missVal"] = TSHeader[0]["pi:stationName"][0]["#text"]
    TSMeta["locationId"] = TSHeader[0]["pi:locationId"][0]["#text"]
    TSMeta["parameterId"] = TSHeader[0]["pi:parameterId"][0]["#text"]
    return TSMeta


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


def getPiTimeSeries(dictree):
    TSSet = {}
    for TSeriesXML in dictree:
        TSHeader = TSeriesXML["pi:header"]
        TSEvent = TSeriesXML["pi:event"]
        TSMeta = getPiTimeSeriesMeta(TSHeader)
        TSDates, TSValues = getDatesValues(TSEvent, TSMeta)
        newSeries = {}
        newSeries["meta"] = TSMeta
        newSeries["dates"] = TSDates
        newSeries["values"] = TSValues
        locationId = TSMeta["locationId"]
        parameterId = TSMeta["parameterId"]
        if parameterId not in TSSet:
            TSSet[parameterId] = {}
        TSSet[parameterId][locationId] = newSeries  # group timeseries by PARAMETER_ID
    return TSSet


class PiTimeseriesComparer(TimeseriesSetComparer.TimeseriesSetComparer):
    """Pi time series comparer.

    Compare two Pi XML Timeseries files, according to the configuration in file_check.
    input: left path (reference), right path (compare), file_check
    output: list of (file_check, parameter, file_check, ResultComparison) tuples.
    """

    def getTimeseriesSet(self, xmlpath):  # override base-class method, delivering a set of timeseries from pi-xml file
        xmltree = minidom.parse(xmlpath)
        dictree = branch(xmltree)
        TSSet = getPiTimeSeries(dictree["pi:TimeSeries"][0]["pi:series"])
        return TSSet
