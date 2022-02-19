"""
Author: Justin Deterding
Created: Thu Feb  6 15:28:56 2020
Description:
"""
# %% Imports
import json
import sys

import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from LogisticGrowthModel import localLogisticGrowth
from matplotlib.patches import Circle
import subprocess

# %% FUNCTION DEFINITIONS
def generatePDFs(X, numberOfBins, binRange=None):
    if not binRange:
        binRange = (X.min(), X.max())
    bins = np.linspace(*binRange, numberOfBins)
    binAssignments = np.digitize(X, bins)
    prob = np.ones(shape=(X.shape[0], numberOfBins))
    for inx, popBinAssignment in enumerate(binAssignments):
        for binNumber in range(1, numberOfBins):
            prob[inx, binNumber] = np.sum(popBinAssignment == binNumber) / X.shape[1]
    return prob


def generateJointPDF(xt, yt, NXbins, NYbins, XbinRange=None, YbinRange=None):
    if not XbinRange:
        XbinRange = (xt.min(), xt.max())
    if not YbinRange:
        YbinRange = (yt.min(), yt.max())
    xbins = np.linspace(*XbinRange, NXbins + 1)
    ybins = np.linspace(*YbinRange, NYbins + 1)
    xtBinAssignments = np.digitize(xt, xbins)
    ytBinAssignments = np.digitize(yt, ybins)
    jointPDF = np.zeros((NXbins, NYbins))
    Xbins, Ybins = np.meshgrid(np.arange(1, NXbins), np.arange(1, NXbins))
    for xi, yi in zip(xtBinAssignments, ytBinAssignments):
        Xbool = np.zeros(Xbins.shape)
        Ybool = np.zeros(Ybins.shape)
        Xbool[Xbins == xi] = 1
        Ybool[Ybins == yi] = 1
        jointPDF += Xbool * Ybool
    jointPDF = jointPDF / len(xt)
    return jointPDF


def entropy(pdf, axis=1):
    with np.errstate(all='ignore'):
        return -1.0 * np.sum(np.nan_to_num(pdf * np.log2(pdf)), axis=axis)


def main():

    f = open('init.json')

    data = json.load(f)
    # %% Parameters
    N_generations = data["N_generations"]
    N_populations = data["N_populations"]
    carryingCapacity = data["carryingCapacity"]
    intiarray =data["k_value"]
    startarray =data["start_value"]
    N = data["N"]
    numberOfBins = data["numberOfBins"]


    R = np.array(intiarray)
    K = np.ones((N_populations,)) * carryingCapacity
    X = np.zeros(shape=(N_populations, N_generations))
    # Initial Populations
    X[:, 0] = np.array(startarray)

    # %% popigate the generations
    for gen in range(N_generations - 1):
        X[:, gen + 1] = localLogisticGrowth(X[:, gen], R, K)
    Xdiff = np.array([np.abs(X[0, :] - X[1, :]), np.abs(X[2, :] - X[3, :])])

    binRange = (0.0, carryingCapacity)
    bins = np.linspace(*binRange, numberOfBins)
    binAssignments = np.digitize(X, bins)


    # %% Save Datae to text for calculations
    np.savetxt('output/fullTimeSeries.txt', binAssignments.T, fmt='%d')
    np.savetxt('output/firstNtimesteps.txt', binAssignments[:, :N].T, fmt='%d')
    np.savetxt('output/lastNtimesteps.txt', binAssignments[:, -N:].T, fmt='%d')


if __name__ == '__main__':
    print("Starting part1 process loop")
    main()
    print("part1 files made. done")
    sys.exit(0)
