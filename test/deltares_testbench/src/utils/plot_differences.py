# coding: utf-8

import logging
import pandas
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
import matplotlib

matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
import os
import numpy


def PlotDifferencesTimeSeries(right_path, date_time, yval1, yval2, case_name, parameter, location, file_type):
    """
    Plot Time series with matplotlib and pandas
    :param right_path: directory to copy plot to (ie test result directory)
    :param date_time: x-axis (format 'yyyy-mm-dd hh:mm:ss' or 'value')
    :param yval1: reference result
    :param yval2: test result
    :param case_name: test case name
    :param parameter:
    :param location:
    :param file_type: Type of the file from which the data is taken
    :return:
    """
    case_name = case_name.strip()
    parameter = parameter.strip()
    location = location.strip()
    file_type = file_type.strip()

    diff = []
    for i in range(0, len(yval1)):
        diff.append(yval2[i] - yval1[i])

    df = pandas.DataFrame({'X': date_time, 'Reference': yval1, 'Test': yval2, 'Diff': diff})
    df.Reference = df.Reference.astype(float)
    df.Test = df.Test.astype(float)
    df.Diff = df.Diff.astype(float)

    fontP = FontProperties()
    fontP.set_size(7)

    y_formatter = matplotlib.ticker.ScalarFormatter(useOffset=False)
    x_formatter = matplotlib.dates.DateFormatter("%Y-%m-%d\n%H:%M:%S", tz=None)

    # generate 2 row, 1 columns
    fig, axes = plt.subplots(2, 1, figsize=(13, 5.5))

    # plot in the two axes
    for name in ['Test', 'Reference']:
        y = df[name]
        style = ':'
        if name == 'Test':
            style = 'r-'
        if name =='Reference':
            style = 'b--'
        axes[0].plot(df['X'], df[name], style, label=name)
    title = 'Test and Reference Results: {case_name}\n{location}'.format(case_name=case_name, location=location)
    axes[0].set_title(title, fontsize=7)
    # axes[0].ticklabel_format(useOffset=False)
    axes[0].set_ylabel(parameter, fontsize=7)
    axes[0].set_xlabel('time $\\quad \\longrightarrow$', fontsize=7)
    axes[0].legend(prop=fontP)
    axes[0].xaxis.set_major_formatter(x_formatter)
    axes[0].xaxis.set_tick_params(labelsize=7)
    axes[0].yaxis.set_major_formatter(y_formatter)
    axes[0].yaxis.set_tick_params(labelsize=7)

    axes[1].plot(df['X'], df['Diff'], 'b-', label='Test - Reference')
    title = 'Difference (Test-Ref): {case_name}\n{location}'.format(case_name=case_name, location=location)
    axes[1].set_title(title, fontsize=7)
    # axes[1].ticklabel_format(useOffset=False)
    axes[1].set_ylabel(parameter, fontsize=7)
    axes[1].set_xlabel('time $\\quad \\longrightarrow$', fontsize=7)
    axes[1].legend(prop=fontP)
    axes[1].xaxis.set_major_formatter(x_formatter)
    axes[1].xaxis.set_tick_params(labelsize=7)
    axes[1].yaxis.set_major_formatter(y_formatter)
    axes[1].yaxis.set_tick_params(labelsize=7)

    fig.tight_layout()

    # save the figure
    logging.debug("Plotting time series, variable: " + str(parameter) + " and location name: " + str(location))
    outfilename = '{parameter}_{location}_{file_type}.pdf'.format(location=location, parameter=parameter, file_type=file_type)
    outfilename = outfilename.replace(r'/', '-')
    outfilename = outfilename.replace(r':', '')
    outfilename = outfilename.replace(r' ', '_')
    outfilename = os.path.join(right_path, outfilename)
    fig.savefig(outfilename)
    fig.clf()
    plt.close('all')


def PlotDifferencesMap(right_path, x, y, yval1, yval2, allowed_diff, case_name, parameter, location, file_type):
    case_name = case_name.strip()
    parameter = parameter.strip()
    location = location.strip()
    file_type = file_type.strip()

    diff = numpy.abs(yval2[:] - yval1[:])
    colours = []
    for i in range(0, len(yval1)):
        colours.append('blue')
    max_diff = max(diff[:])
    dotsize = 5.
    area = diff / max_diff * dotsize
    for i in range(len(yval1)):
        if area[i] > 0.8 * dotsize:
            colours[i] = 'red'
        elif area[i] > allowed_diff / max_diff * dotsize:
            colours[i] = 'grey'
            area[i] = max(0.15, area[i])
        if area[i] <= allowed_diff / max_diff * dotsize:
            area[i] = 0.15

    y_formatter = matplotlib.ticker.ScalarFormatter(useOffset=False)
    fig = plt.figure()
    title = 'Abs. Difference (Test-Ref): {case_name}\n {parameter} {location}'.format(case_name=case_name, location=location, parameter=parameter)
    ax1 = fig.add_subplot(111)

    ratio = ax1.get_data_ratio()
    dx = float(numpy.max(x)) - float(numpy.min(x))
    dy = float(numpy.max(y)) - float(numpy.min(y))
    if numpy.isclose(dx, 0.0):  # all x-coordinates are the same, straight 1d channel
        dx = 1.0
        if dy / dx < 0.10:
            ratio = 0.1 * dy / dx
        if dy / dx > 10.0:
            ratio = 10.0 * dy / dx
    else:
        if dy / dx < 0.10:
            ratio = 0.1
        if dy / dx > 10.0:
            ratio = 10.0

    if numpy.isclose(dy, 0.0):  # all y-coordinates are the same, straight 1d channel
        dy = 1.0
        if dy / dx < 0.10:
            ratio = 0.1 * dy / dx
        if dy / dx > 10.0:
            ratio = 10.0 * dy / dx
    else:
        if dy / dx < 0.10:
            ratio = 0.1
        if dy / dx > 10.0:
            ratio = 10.0

    ax1.set_aspect(1. / ratio)  # make axes square
    ax1.set_title(title, fontsize=7)
    ax1.yaxis.set_major_formatter(y_formatter)

    plt.xlabel('$x \\quad \\longrightarrow$', fontsize=7)
    plt.ylabel('$y \\quad \\longrightarrow$', fontsize=7)
    plt.tick_params(axis='both', which='major', labelsize=7)
    plt.tick_params(axis='both', which='minor', labelsize=7)

    tekst = ('Dotsize represents difference\nGiven abs. tolerance: %f\nMax. abs. difference: %f\nRed dot > 0.8*%f\nGrey dot: above tolerance\nBlue dot: below tolerance' % (allowed_diff, max_diff, max_diff))
    fig.text(0.01, 0.90, tekst, fontsize=5)

    ax1.scatter(x, y, c=colours, s=area, edgecolor='none')

    # save the figure
    logging.debug("Plotting map, variable: " + parameter + " and location name: " + location)
    outfilename = '{parameter}_{location}_{file_type}.pdf'.format(location=location, parameter=parameter, file_type=file_type)
    outfilename = outfilename.replace(r'/', '-')
    outfilename = outfilename.replace(r':', '')
    outfilename = outfilename.replace(r' ', '_')
    outfilename = os.path.join(right_path, outfilename)
    plt.savefig(outfilename)
    plt.clf()
    plt.close('all')
