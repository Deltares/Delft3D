import numpy as np
import matplotlib.pyplot as plt
import os

numTestCases = 6  # num test cases
numRuns = 3  # num runs per test case
numParams = 5  # num parameters on a line

timingFile = "./history/timing.history"

timings_file = open(timingFile)

##########################################
# Get results and put into array
lst = []
lst = [section.split() for section in timings_file.read().split("\n\n")][0]
timings_file.close()

numResults = int(len(lst) / numParams)

runTimes = np.zeros((numTestCases, numRuns, int(numResults / (numRuns * numTestCases))))


for nTc in range(0, numTestCases):
    for nR in range(0, numRuns):
        runTimes[nTc, nR, :] = lst[
            3
            + nR * numParams
            + nTc * numRuns * numParams :: numTestCases * numRuns * numParams
        ]


##########################################
# Get references and put into array
referenceRunTimes = np.zeros((numTestCases, numRuns))

for nTc in range(0, numTestCases):
    testcase = lst[numParams - 1 + numRuns * numParams * nTc]
    referenceTimingFile = os.path.join(
        "d:/checkouts/references_DSCTestbench/lnx64/e02_dflowfm/f031_performance_transport",
        testcase,
        "timing.out",
    )

    reference_timings_file = open(referenceTimingFile)
    refLst = []
    refLst = [
        section.split() for section in reference_timings_file.read().split("\n\n")
    ][0]
    for nR in range(0, numRuns):
        referenceRunTimes[nTc, nR] = refLst[int(len(refLst) / numRuns * (nR + 1) - 1)]
    reference_timings_file.close()


##########################################
margin = 1.10
print("Means reported with margin, ",margin)
# Plots
for nTc in range(0, numTestCases):
    testcase = lst[numParams - 1 + numRuns * numParams * nTc]
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)

    # Plot results
    for nR in range(0, numRuns):
        ax.plot(
            runTimes[nTc, nR, :],
            label=lst[2 + nR * numParams] + " tracers",
            marker="o",
            color="C" + str(nR),
        )
        print(testcase, "run " + str(nR), format(margin*np.mean(runTimes[nTc, nR, :]), ".0f"))

    # Plot reference results
    for nR in range(0, numRuns):
        ax.hlines(
            referenceRunTimes[nTc, nR],
            0,
            numResults / (numRuns * numTestCases),
            label="ref. " + lst[2 + nR * numParams] + " tracers",
            linestyle="-.",
            color="C" + str(nR),
        )

    ax.set_title(lst[numRuns * numParams * (nTc + 1) - 1])
    ax.legend()
    ax.set_xlabel("# run")
    ax.set_ylabel("run time / s")
    plt.savefig("timing_" + testcase + ".png")


plt.show()
