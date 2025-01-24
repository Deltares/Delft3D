//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dimr.cpp 962 2011-10-31 21:52:47Z elshoff $
// $HeadURL: $
//------------------------------------------------------------------------------
//  dimr Main Program
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------

#define DIMR_LIB
#include <string>
#include <iostream>
#include <set>

using namespace std;

#include "dimr.h"
#include "dimr_lib_version.h"

#ifndef _WIN32
#include "config.h"
#include <dlfcn.h>
#include <libgen.h>
#endif
#include <expat.h>
#include <limits.h>
#include <time.h>

#if defined (MEMCHECK)
#include <mcheck.h>
#endif


#include <typeinfo>
//#include <filesystem>
using namespace std;

#include <string>
#include <sstream>
#include <math.h>
#include <mpi.h>
#include <netcdf.h>
#include <ctime>
# include <stdio.h>
#include <fstream>
#include <iomanip>

#if defined (WIN32)
//#  include "getopt.h"
#  include <Strsafe.h>
#  include <windows.h>
#  include <direct.h>
#include <errno.h>
#  include <io.h>
#  include <sys/stat.h>

#  define strdup _strdup
#  define chdir _chdir
#  define getcwd _getcwd
#  define dup2 _dup2
#else
#  include <unistd.h>
#endif

Dimr* Dimr::instance = NULL;

Dimr::Dimr(void) {
    FILE* logFile = stdout;
    ready = false;
    exePath = NULL;
    exeName = NULL;
    clock = new Clock();
    logLevel = WARNING;
    feedbackLevel = INFO;
    log = new Log(logFile, clock, logLevel, feedbackLevel);
    config = NULL;
    mainArgs = NULL;
    slaveArg = NULL;
    control = NULL;
    componentsList.numComponents = 0;
    couplersList.numCouplers = 0;
    use_mpi = false;
    my_rank = 0;
    numranks = 1;
    configfile = NULL;
    done = false;
    // Initialize redirectFile. Default: switched On (!=NULL)
    const char* filename = "dimr_redirected.log";
    int len = strlen(filename);
    redirectFile = (char*)malloc((len + 1) * sizeof(char));
    strncpy(redirectFile, (const char*)filename, len);
    redirectFile[len] = '\0';
#ifndef _WIN32
    dirSeparator = "/";
#else
    dirSeparator = "\\";
#endif
    // Do not remove the next two lines; they ensure that the version numbers are added to the binary
    char* dimrlibversion = getversionidstring_dimr_lib();
    char* dimrurl = geturlstring_dimr_lib();
    this->timerStartStamp = clock->Epoch();
    this->timerSumStamp = 0;
}

bool readComputeTimesFile(const char* fileName, dimr_control_block* controlBlock) {
    std::ifstream computeTimesFile(fileName);
    // Check if the file stream is valid (i.e., file was opened successfully)
    if (!computeTimesFile.is_open()) {
        return false;
    }
    double timeRead;
    controlBlock->computeTimes = new vector<double>();
    while (computeTimesFile >> timeRead)
        controlBlock->computeTimes->push_back(timeRead);
    // Close the file
    computeTimesFile.close();
    vector<double>& controlblock_timearray = *(controlBlock->computeTimes); // Shortcut to keep the code readable
    // Enough data?
    if (controlblock_timearray.size() < MINIMUM_TIME_POINTS)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "File '%s' must contain at least %d times", fileName, MINIMUM_TIME_POINTS);
    // Update controlBlock with read times
    controlBlock->tStart = controlblock_timearray[0];                       // First  timePoint to do a computation
    controlBlock->tStep = controlblock_timearray[1] - controlBlock->tStart; // Second timePoint to do a computation
    controlBlock->computeTimesCurrent = 1;                                  // Index to current timePoint
    controlBlock->tEnd = std::numeric_limits<double>::infinity();
    return true;
}


//------------------------------------------------------------------------------
int dimr_component::dllSetKeyVals(keyValueLL* kv) {
    // Pass parameters for the first controll block's component: parameters
    int count = 0;
    while (kv) {
        if (dllSetVar != NULL) {
            (dllSetVar)(kv->key, (void*)kv->val);
        }
        else {
            if (dllGetVar != NULL) {
                (dllGetVar)(kv->key, (void**)kv->val);
            }
        }
        kv = kv->nextkv;
        count++;
    }
    return count;
}


//
//------------------------------------------------------------------------------
//  Destructor
Dimr::~Dimr(void) {
    if (done)
        return;

    // to do:  (void) FreeLibrary(handle);
    freeLibs();

    log->Write(DEBUG, my_rank, "dimr shutting down normally");

#ifndef _WIN32
    free(exeName);
#else
    delete[] exeName;
#endif

    delete clock;
    delete config;
    free(exePath);
    delete log;
    delete[] mainArgs;
    // componentsList
    delete[] componentsList.components;
    // couplersList
    for (int i = 0; i < couplersList.numCouplers; i++) {
        delete[] couplersList.couplers[i].items;
        if (couplersList.couplers[i].logger != NULL) {
            delete couplersList.couplers[i].logger;
        }
    }
    delete[] couplersList.couplers;
    // control
    if (control != NULL)
        deleteControlBlock(*(control));
    free(control);
    done = true;
}



//------------------------------------------------------------------------------
void Dimr::deleteControlBlock(dimr_control_block cb) {
    if (cb.numSubBlocks > 0) {
        for (int i = 0; i < cb.numSubBlocks; i++) {
            if (cb.computeTimes->empty())
                cb.computeTimes->clear();
            // Recursively delete all subBlocks
            deleteControlBlock(cb.subBlocks[i]);
        }
        delete[] cb.subBlocks;
    }
}




// WARNING: dimr is not BMI compliant yet!
// tStep is not used in runControlBlock
//------------------------------------------------------------------------------
void Dimr::runControlBlock(dimr_control_block* cb, double tStep, int phase) {
    if (cb->type == CT_PARALLEL) {
        log->Write(INFO, my_rank, "PARALLEL:");
        //
        //
        // Initialize loop
        if (phase == GLOBAL_PHASE_FINISH) {
            runParallelInit(cb);
        }
        //
        //
        // Update loop
        runParallelUpdate(cb, tStep);
        //
        //
        // Finalize loop
        if (phase == GLOBAL_PHASE_FINISH) {
            runParallelFinish(cb);
        }
    }
    else if (cb->type == CT_START) {
        runStartBlock(cb, tStep, phase);
    }
    fflush(stdout);
}



//------------------------------------------------------------------------------
void Dimr::runStartBlock(dimr_control_block* cb, double tStep, int phase) {
    log->Write(INFO, my_rank, "START:");

    chdir(cb->unit.component->workingDir);
    if (phase == GLOBAL_PHASE_FINISH) {
        log->Write(INFO, my_rank, "%s.Initialize(%s)", cb->unit.component->name, cb->unit.component->inputFile);
        timerStart(cb->unit.component);
        int nSettingsSet = cb->unit.component->dllSetKeyVals(cb->unit.component->settings);
        cb->unit.component->result = (cb->unit.component->dllInitialize) (cb->unit.component->inputFile);
        if (cb->unit.component->result != 0)
        {
            stringstream ss;
            ss << cb->unit.component->result;
            std::string componentName = cb->unit.component->name;
            std::string message = "#### ERROR: dimr initialize ABORT,: " + componentName + " initialize failed, with return value " + ss.str() + " \n";
            printf(message.c_str());
            log->Write(FATAL, my_rank, message.c_str(), configfile);
            throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
        }
        timerEnd(cb->unit.component);
        (cb->unit.component->dllGetStartTime) (&cb->tStart);
        (cb->unit.component->dllGetEndTime) (&cb->tEnd);
    }
    cb->tStep = tStep;
    log->Write(INFO, my_rank, "%s.Update(%6.1f)", cb->unit.component->name, cb->tStep);
    timerStart(cb->unit.component);
    int state = (cb->unit.component->dllUpdate) (cb->tStep);
    if (state != 0)
    {
        stringstream ss, curTime;
        ss << state;
        curTime << *&cb->tCur;
        std::string componentName = cb->unit.component->name;
        std::string message = "#### ERROR: dimr update ABORT,: " + componentName + " update failed, with return value " + ss.str() + " at time : " + curTime.str() + "\n";
        printf(message.c_str());
        log->Write(FATAL, my_rank, message.c_str(), configfile);
        throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
    }
    timerEnd(cb->unit.component);
    if (phase == GLOBAL_PHASE_FINISH) {
        log->Write(INFO, my_rank, "%s.Finalize()", cb->unit.component->name);
        timerStart(cb->unit.component);
        int state = (cb->unit.component->dllFinalize) ();
        if (state != 0)
        {
            stringstream ss;
            ss << state;
            std::string componentName = cb->unit.component->name;
            std::string message = "#### ERROR: dimr finalize ABORT,: " + componentName + " finalize failed, with return value " + ss.str() + " \n";
            printf(message.c_str());
            log->Write(FATAL, my_rank, message.c_str(), configfile);
            throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
        }
        timerEnd(cb->unit.component);
    }
    fflush(stdout);
}



//------------------------------------------------------------------------------
void Dimr::runParallelInit(dimr_control_block* cb) {
    int ierr;
    MPI_Group mpiGroupWorld;
    MPI_Group mpiGroupComp;
    int nSettingsSet;

    // RTCTools/Wanda/Flow1D2D: impossible to autodetect which partition will deliver this source var
    //              Assumption: there is only one RTC/Wanda/Flow1D2D/ZSF-instance
    std::set<int> single_instance_component_set = { COMP_TYPE_RTC, COMP_TYPE_WANDA, COMP_TYPE_FLOW1D2D, COMP_TYPE_ZSF};

    if (use_mpi) {
        ierr = MPI_Comm_group(MPI_COMM_WORLD, &mpiGroupWorld);
        if (ierr != MPI_SUCCESS) {
            throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot obtain MPI world group. Code: %d.", ierr);
        }
    }

    // set masterSubBlockId
    for (int i = 0; i < cb->numSubBlocks; i++) {
        if (cb->subBlocks[i].type == CT_START) {
            if (cb->masterSubBlockId == -1) {
                cb->masterSubBlockId = i;
                log->Write(DEBUG, my_rank, "Master: %s", cb->subBlocks[cb->masterSubBlockId].unit.component->name);
            }
            else {
                throw Exception(true, Exception::ERR_INVALID_INPUT, "runParallelInit: a parallel block cannot have more than one start element.");
            }
        }
    }
    if (cb->masterSubBlockId == -1)
    {
        throw Exception(true, Exception::ERR_INVALID_INPUT, "runParallelInit: a parallel block must have at least one start element.");
    }

    // Hack:
    // The masterComponent must be initialized first
    // Wave can only be initialized after the flow component
    dimr_component* masterComponent = cb->subBlocks[cb->masterSubBlockId].unit.component;

    // Create an MPI subgroup and subcommunicator and pass it on to the masterComponent.
    if (use_mpi && masterComponent->mpiCommVar != NULL) {
        ierr = MPI_Group_incl(mpiGroupWorld, masterComponent->numProcesses, masterComponent->processes, &mpiGroupComp);
        if (ierr != MPI_SUCCESS) {
            throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot create a subgroup of %d processes for component \"%s\". Code: %d.", masterComponent->numProcesses, masterComponent->name, ierr);
        }
        // Needs to be called by *all* ranks:
        ierr = MPI_Comm_create(MPI_COMM_WORLD, mpiGroupComp, &masterComponent->mpiComm);
        if (ierr != MPI_SUCCESS) {
            throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot create a subcommunicator of %d processes for component \"%s\". Code: %d.", masterComponent->numProcesses, masterComponent->name, ierr);
        }
        if (masterComponent->onThisRank) {
            MPI_Fint* fComm;
            masterComponent->dllGetVar(masterComponent->mpiCommVar, (void**)(&fComm));
            if (fComm == NULL) {
                throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot obtain reference to communicator handle \"%s\" from component \"%s\".", masterComponent->mpiCommVar, masterComponent->name);
            }
            *fComm = MPI_Comm_c2f(masterComponent->mpiComm);
        }
    }
    if (masterComponent->onThisRank) {
        chdir(masterComponent->workingDir);
        log->Write(INFO, my_rank, "%s.Initialize(%s)", masterComponent->name, masterComponent->inputFile);
                // SetKeyVals for settings (before initialize)
                int nSettingsSet = masterComponent->dllSetKeyVals(masterComponent->settings);
        timerStart(masterComponent);
        masterComponent->result = (masterComponent->dllInitialize) (masterComponent->inputFile);
        if (masterComponent->result != 0)
        {
            stringstream ss;
            ss << masterComponent->result;
            std::string componentName = masterComponent->name;
            std::string message = "#### ERROR: dimr initialize ABORT,: " + componentName + " initialize failed, with return value " + ss.str() + " \n";
            printf(message.c_str());
            log->Write(FATAL, my_rank, message.c_str(), configfile);
            throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
        }
        timerEnd(masterComponent);
                // SetKeyVals for parameters (after initialize)
                int nParamsSet = masterComponent->dllSetKeyVals(masterComponent->parameters);
        (masterComponent->dllGetStartTime) (&cb->subBlocks[cb->masterSubBlockId].tStart);
        (masterComponent->dllGetEndTime) (&cb->subBlocks[cb->masterSubBlockId].tEnd);
        (masterComponent->dllGetTimeStep) (&cb->subBlocks[cb->masterSubBlockId].tStep);
        (masterComponent->dllGetCurrentTime) (&cb->subBlocks[cb->masterSubBlockId].tCur);
    }

    if (masterComponent->numProcesses < numranks) {
        double dbl4_buf[4];

        if (masterComponent->processes[0] == my_rank) {
            dbl4_buf[0] = cb->subBlocks[cb->masterSubBlockId].tStart;
            dbl4_buf[1] = cb->subBlocks[cb->masterSubBlockId].tEnd;
            dbl4_buf[2] = cb->subBlocks[cb->masterSubBlockId].tStep;
            dbl4_buf[3] = cb->subBlocks[cb->masterSubBlockId].tCur;
        }
        else {
            for (int i = 0; i < 4; i++)
            {
                dbl4_buf[i] = 0.0;
            }
        }
        ierr = MPI_Bcast(dbl4_buf, 4, MPI_DOUBLE, masterComponent->processes[0], MPI_COMM_WORLD);

        cb->subBlocks[cb->masterSubBlockId].tStart = dbl4_buf[0];
        cb->subBlocks[cb->masterSubBlockId].tEnd = dbl4_buf[1];
        cb->subBlocks[cb->masterSubBlockId].tStep = dbl4_buf[2];
        cb->subBlocks[cb->masterSubBlockId].tCur = dbl4_buf[3];
    }

    //
    // Then initialize the other components and couplers
    for (int i = 0; i < cb->numSubBlocks; i++) {
        if (i != cb->masterSubBlockId) {
            // CT_STARTGROUP
            // First all components
            for (int j = 0; j < cb->subBlocks[i].numSubBlocks; j++) {
                if (cb->subBlocks[i].subBlocks[j].type == CT_START) {
                    dimr_component* thisComponent = cb->subBlocks[i].subBlocks[j].unit.component;

                    // Create an MPI subgroup and subcommunicator and pass it on to thisComponent (similar to block for masterComponent above).
                    if (use_mpi && thisComponent->mpiCommVar != NULL && thisComponent->numProcesses > 1) { // TODO: consider removing the numproc>1 check.
                        ierr = MPI_Group_incl(mpiGroupWorld, thisComponent->numProcesses, thisComponent->processes, &mpiGroupComp);
                        if (ierr != MPI_SUCCESS) {
                            throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot create a subgroup of %d processes for component \"%s\". Code: %d.", thisComponent->numProcesses, thisComponent->name, ierr);
                        }
                        // Needs to be called by *all* ranks:
                        ierr = MPI_Comm_create(MPI_COMM_WORLD, mpiGroupComp, &thisComponent->mpiComm);
                        if (ierr != MPI_SUCCESS) {
                            throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot create a subcommunicator of %d processes for component \"%s\". Code: %d.", thisComponent->numProcesses, thisComponent->name, ierr);
                        }
                        if (thisComponent->onThisRank) {
                            MPI_Fint* fComm;
                            thisComponent->dllGetVar(thisComponent->mpiCommVar, (void**)(&fComm));
                            if (fComm == NULL) {
                                throw Exception(true, Exception::ERR_MPI, "runParallelInit: cannot obtain reference to communicator handle \"%s\" from component \"%s\".", thisComponent->mpiCommVar, thisComponent->name);
                            }
                            *fComm = MPI_Comm_c2f(thisComponent->mpiComm);
                        }
                    }

                    if (thisComponent->onThisRank) { // TODO: AvD/AM: if FM is not start, but startblock, we need all the MPI stuff here as well: make a generic initializeComponent helper routine.

                        // Hack for WAVE:
                        if (thisComponent->type == COMP_TYPE_WAVE) {
                            int* waveModePtr = NULL;
                            const char* key = "mode";
                            (thisComponent->dllGetVar) (key, (void**)(&waveModePtr));
                            *waveModePtr = 1;
                        }

                        chdir(thisComponent->workingDir);
                        log->Write(INFO, my_rank, "%s.Initialize(%s)", thisComponent->name, thisComponent->inputFile);
                                                // SetKeyVals for settings (before initialize)
                                                int nSettingsSet = thisComponent->dllSetKeyVals(thisComponent->settings);
                        timerStart(thisComponent);
                        thisComponent->result = (thisComponent->dllInitialize) (thisComponent->inputFile);
                        if (thisComponent->result != 0)
                        {
                            stringstream ss;
                            ss << thisComponent->result;
                            std::string componentName = thisComponent->name;
                            std::string message = "#### ERROR: dimr initialize ABORT,: " + componentName + " initialize failed, with return value " + ss.str() + " \n";
                            printf(message.c_str());
                            log->Write(FATAL, my_rank, message.c_str(), configfile);

                            throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
                        }
                        timerEnd(thisComponent);
                                                // SetKeyVals for parameters (after initialize)
                                                int nParamsSet = thisComponent->dllSetKeyVals(thisComponent->parameters);
                    }
                }
            }
            // Then all couplers
            for (int j = 0; j < cb->subBlocks[i].numSubBlocks; j++) {
                if (cb->subBlocks[i].subBlocks[j].type != CT_START) {
                    dimr_coupler* thisCoupler = cb->subBlocks[i].subBlocks[j].unit.coupler;
                    for (int k = 0; k < thisCoupler->numItems; k++) {
                        if (IsCouplerItemTypePTR(thisCoupler->itemTypes[k]) ||
                            single_instance_component_set.find(thisCoupler->sourceComponent->type) != single_instance_component_set.end()) {
                            // ITEM_TYPE_PTR : Arrays to point to are not yet allocated
                            //                 Assumption: they will be when the data actually will be communicated
                            //                 Warning: do not change this into "Component->type == COMP_TYPE_COSUMO",
                            //                          because it will also fail for the related DflowFM item
                            thisCoupler->items[k].sourceProcess = thisCoupler->sourceComponent->processes[0];
                        }
                        else {
                            // For each item: get the pointers to the variables inside the dlls to be exchanged
                            // Currently this does not work for RTC-Tools
                            //
                            // Source variable
                            // autodetect which (single!) partition will deliver this source var
                            int* sources = (int*)malloc(thisCoupler->sourceComponent->numProcesses * sizeof(int));
                            int* gsources = (int*)malloc(thisCoupler->sourceComponent->numProcesses * sizeof(int));
                            for (int m = 0; m < thisCoupler->sourceComponent->numProcesses; m++) {
                                sources[m] = 0;
                                if (my_rank == thisCoupler->sourceComponent->processes[m]) {
                                    // Also for RTCTools_BMI: this is a dummy getvar call, just to check whether it works for this partition
                                    log->Write(DEBUG, my_rank, "%s.getVar(%s)", thisCoupler->sourceComponentName, thisCoupler->items[k].sourceName);
                                    (thisCoupler->sourceComponent->dllGetVar) (thisCoupler->items[k].sourceName, (void**)(&thisCoupler->items[k].sourceVarPtr));
                                    if (thisCoupler->items[k].sourceVarPtr != NULL) {
                                        // Yes, this partition can deliver the source var
                                        sources[m] = 1;
                                    }
                                }
                            }
                            // Do not call MPI_Allreduce when the number of partitions is 1. It will cause a crash on free(gsources)
                            if (numranks > 1) {
                                int ierr = MPI_Allreduce(sources, gsources, thisCoupler->sourceComponent->numProcesses, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
                            }
                            else {
                                for (int m = 0; m < thisCoupler->sourceComponent->numProcesses; m++) {
                                    gsources[m] = sources[m];
                                }
                            }
                            thisCoupler->items[k].sourceProcess = -1;
                            for (int m = 0; m < thisCoupler->sourceComponent->numProcesses; m++) {
                                if (gsources[m] == 1) {
                                    if (thisCoupler->items[k].sourceProcess == -1) {
                                        // First partition that can deliver the source var
                                        thisCoupler->items[k].sourceProcess = m;
                                    }
                                    else {
                                        // Second/Third/... partition that can deliver the source var
                                        // Produce a warning
                                        // The "if (my_rank == m)" avoids multiple identical messages
                                        if (my_rank == m) {
                                            log->Write(WARNING, my_rank, "WARNING: coupler %s: item %d: \"%s\" will be delivered by partition %d. Ignoring deliverance by partition %d",
                                                thisCoupler->name, k, thisCoupler->items[k].sourceName, thisCoupler->items[k].sourceProcess, m);
                                        }
                                    }
                                }
                            }
                            free(sources);
                            free(gsources);
                        }

                        // Target variable
                        if (IsCouplerItemTypePTR(thisCoupler->itemTypes[k]) ||
                            single_instance_component_set.find(thisCoupler->targetComponent->type) != single_instance_component_set.end()) {
                            // nothing
                        }
                        else {
                            // Target variable
                            // autodetect which (possibly multiple!) partition(s) will accept this target var
                            int* targets = (int*)malloc(thisCoupler->targetComponent->numProcesses * sizeof(int));
                            int* gtargets = (int*)malloc(thisCoupler->targetComponent->numProcesses * sizeof(int));
                            for (int m = 0; m < thisCoupler->targetComponent->numProcesses; m++) {
                                targets[m] = 0;
                                if (my_rank == thisCoupler->targetComponent->processes[m]) {
                                    log->Write(DEBUG, my_rank, "%s.getVar(%s)", thisCoupler->targetComponentName, thisCoupler->items[k].targetName);
                                    (thisCoupler->targetComponent->dllGetVar) (thisCoupler->items[k].targetName, (void**)(&thisCoupler->items[k].targetVarPtr));
                                    if (thisCoupler->items[k].targetVarPtr != NULL) {
                                        // Yes, this partition can accept the target var
                                        targets[m] = 1;
                                    }
                                }
                            }
                            // Do not call MPI_Allreduce when the number of partitions is 1. It will cause a crash on free(gtargets)
                            if (numranks > 1) {
                                int ierr = MPI_Allreduce(targets, gtargets, thisCoupler->targetComponent->numProcesses, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
                            }
                            else {
                                for (int m = 0; m < thisCoupler->targetComponent->numProcesses; m++) {
                                    gtargets[m] = targets[m];
                                }
                            }
                            thisCoupler->items[k].targetProcess = -1;
                            for (int m = 0; m < thisCoupler->targetComponent->numProcesses; m++) {
                                if (gtargets[m] == 1) {
                                    if (thisCoupler->items[k].targetProcess == -1) {
                                        // First partition that can accept the target var
                                        thisCoupler->items[k].targetProcess = m;
                                    }
                                    else {
                                        // Second/Third/... partition that can accept the target var
                                        // Produce a warning
                                        // The "if (my_rank == m)" avoids multiple identical messages
                                        if (my_rank == m) {
                                            log->Write(WARNING, my_rank, "WARNING: coupler %s: item %d: \"%s\" will be delivered to multiple partitions: %d and %d.",
                                                thisCoupler->name, k, thisCoupler->items[k].targetName, thisCoupler->items[k].targetProcess, m);
                                        }
                                    }
                                }
                            }
                            if (thisCoupler->items[k].targetProcess == -1) {
                                throw Exception(true, Exception::ERR_MPI, "Coupler %s: item %d: \"%s\" is not accepted by any of the partitions.",
                                    thisCoupler->name, k, thisCoupler->items[k].targetName);
                            }
                            free(targets);
                            free(gtargets);
                        }
                    }

                    // create netcdf logfiles
                    // This must be moved to DELTARES_COMMON_C
                    if (thisCoupler->logger != NULL && my_rank == 0)
                    {
                        // create netcdf file in workingdir

                        string fileName = thisCoupler->logger->GetLoggerFilename(dimrWorkingDirectory, dirSeparator);

                        // write NetCDF file

                        int ncid = -1;
                        if (nc_create(fileName.c_str(), nc_mode, &ncid))
                            throw Exception(true, Exception::ERR_OS, "Could not create NetCDF file at location \"%s\".", fileName.c_str());
                        ncfiles[fileName] = ncid;

                        // write global attributes

                        time_t now;
                        time(&now);
                        char buf[sizeof("2017-10-10T16:57:32Z") + 1];
                        strftime(buf, sizeof buf, "%Y-%m-%dT%H:%M:%SZ", gmtime(&now));

                        const char institution[] = "Deltares";
                        nc_put_att_text(ncid, NC_GLOBAL, "institution", strlen(institution), institution);
                        const char references[] = "https://www.deltares.nl";
                        nc_put_att_text(ncid, NC_GLOBAL, "references", strlen(references), references);
                        std::ostringstream source;
                        source << "DIMR " << getfullversionstring_dimr_lib();
                        string sourcestr(source.str());
                        nc_put_att_text(ncid, NC_GLOBAL, "source", sourcestr.size(), sourcestr.c_str());
                        std::ostringstream history;
                        history << "Created on " << buf << ", DIMR.";
                        string historystr(history.str());
                        nc_put_att_text(ncid, NC_GLOBAL, "history", historystr.size(), historystr.c_str());
                        std::ostringstream title;
                        const char* version = "version";
                        char* sourceComponentVersion = new char[1024];
                        char* targetComponentVersion = new char[1024];
                        strcpy(sourceComponentVersion, "");
                        strcpy(targetComponentVersion, "");
                        if (thisCoupler->sourceComponent->dllGetAttribute != NULL) {
                            thisCoupler->sourceComponent->dllGetAttribute(version, sourceComponentVersion);
                        }
                        if (thisCoupler->targetComponent->dllGetAttribute != NULL) {
                            thisCoupler->targetComponent->dllGetAttribute(version, targetComponentVersion);
                        }
                        if (strlen(sourceComponentVersion) == 0) {
                            strcpy(sourceComponentVersion, "Unknown");
                        }
                        if (strlen(targetComponentVersion) == 0) {
                            strcpy(targetComponentVersion, "Unknown");
                        }
                        const string sourceComponentName(thisCoupler->sourceComponentName);
                        const string targetComponentName(thisCoupler->targetComponentName);
                        const string sourceComponentVersionStr(sourceComponentVersion);
                        const string targetComponentVersionStr(targetComponentVersion);
                        title << "Data transferred from " << sourceComponentName << " " << sourceComponentVersionStr
                            << " to " << targetComponentName << " " << targetComponentVersionStr;
                        string titlestr(title.str());
                        nc_put_att_text(ncid, NC_GLOBAL, "title", titlestr.size(), titlestr.c_str());
                        delete[] sourceComponentVersion;
                        delete[] targetComponentVersion;
                        const char conventions[] = "CF-1.7";
                        nc_put_att_text(ncid, NC_GLOBAL, "Conventions", strlen(conventions), conventions);
                        const char feature_type[] = "timeSeries";
                        nc_put_att_text(ncid, NC_GLOBAL, "featureType", strlen(feature_type), feature_type);

                        // write dimensions

                        nc_def_dim(ncid, "strlen", 256, &thisCoupler->logger->netcdfReferences->strlenDim);
                        nc_def_dim(ncid, "time_offset", NC_UNLIMITED, &thisCoupler->logger->netcdfReferences->timeDim);

                        nc_def_var(ncid, "time_offset", NC_DOUBLE, 1, &thisCoupler->logger->netcdfReferences->timeDim, &thisCoupler->logger->netcdfReferences->timeVar);
                        const char longnametime[] = "seconds since simulation reference date, T00:00:00";
                        nc_put_att_text(ncid, thisCoupler->logger->netcdfReferences->timeVar, "long_name", sizeof(longnametime), longnametime);
                        const char units[] = "seconds since 1980-01-01T00:00:00"; // TODO: Get simulation reference time from one of the kernels
                        nc_put_att_text(ncid, thisCoupler->logger->netcdfReferences->timeVar, "units", sizeof(units), units);
                        const char axis[] = "T";
                        nc_put_att_text(ncid, thisCoupler->logger->netcdfReferences->timeVar, "axis", sizeof(axis), axis);

                        // write variables/parameter

                        thisCoupler->logger->netcdfReferences->item_values = new int[thisCoupler->numItems];
                        thisCoupler->logger->netcdfReferences->item_variables = new int[thisCoupler->numItems];
                        for (int k = 0; k < thisCoupler->numItems; k++)
                        {
                            std::ostringstream oss;
                            oss << "item" << k + 1 << "_nValues";
                            const string valuestr(oss.str());
                            int status = nc_def_dim(ncid, valuestr.c_str(), 1, &thisCoupler->logger->netcdfReferences->item_values[k]);
                            if (status != NC_NOERR) {
                                throw Exception(true, Exception::ERR_OS, "Could not create dimension \"%s\".", valuestr.c_str());
                            }

                            int dimensions[2] = { thisCoupler->logger->netcdfReferences->timeDim, thisCoupler->logger->netcdfReferences->item_values[k] };
                            int dummyVar;
                            std::ostringstream varName;
                            varName << "item" << k + 1 << "_values";
                            const string varnamestr(varName.str());
                            nc_def_var(ncid, varnamestr.c_str(), NC_DOUBLE, 2, dimensions, &thisCoupler->logger->netcdfReferences->item_variables[k]);

                            std::ostringstream itemValuesLongName;
                            const string sourceName = string(thisCoupler->items[k].sourceName);
                            itemValuesLongName << sourceName
                                << " -> " << string(thisCoupler->items[k].targetName);
                            const string itemvaluesstr(itemValuesLongName.str());
                            nc_put_att_text(ncid, thisCoupler->logger->netcdfReferences->item_variables[k], "long_name", itemvaluesstr.size(), itemvaluesstr.c_str());

                            std::ostringstream itemValuesCoordinates;
                            itemValuesCoordinates << "station_name";
                            const string itemValuesCoordinatesstr(itemValuesCoordinates.str());
                            nc_put_att_text(ncid, thisCoupler->logger->netcdfReferences->item_variables[k], "coordinates", itemValuesCoordinatesstr.size(), itemValuesCoordinatesstr.c_str());
                        }

                        // write location
                        long nStations = 1;
                        long name_strlen = 64;
                        int id_nStations;
                        int id_name_strlen;
                        int station_var;
                        {
                            nc_def_dim(ncid, "nStations", nStations, &id_nStations);
                            nc_def_dim(ncid, "name_strlen", name_strlen, &id_name_strlen);

                            int dim_id[2];
                            dim_id[0] = id_nStations;
                            dim_id[1] = id_name_strlen;
                            nc_def_var(ncid, "station_name", NC_CHAR, 2, dim_id, &station_var);
                            const char timeseries_id[] = "timeseries_id";
                            nc_put_att_text(ncid, station_var, "cf_role", strlen(timeseries_id), timeseries_id);
                            const char longName[] = "Station name";
                            nc_put_att_text(ncid, station_var, "long_name", strlen(longName), longName);
                        }

                        nc_enddef(ncid);

                        // Add station name
                        std::string varName = sourceComponentName + " -> " + targetComponentName;
                        varName.append(name_strlen - varName.length(), ' ');
                        nc_put_var_text(ncid, station_var, varName.c_str());
                    }
                }
            }
        }
    }
}



//------------------------------------------------------------------------------
void Dimr::runParallelUpdate(dimr_control_block* cb, double tStep) {
    dimr_control_block* masterComponent = &cb->subBlocks[cb->masterSubBlockId];
    // Initialize time parameters
    double* currentTime = &masterComponent->tCur;

    log->Write(INFO, my_rank, "Parallel Update: Current time: %15.5f -- %s -- %d", *currentTime, masterComponent->unit.component->name, tStep);

    // already requested in "runParallelInit" ... is this really nessary?
    // masterComponent->tStart : simulation start time as obtained from the masterComponent
    // masterComponent->tEnd   : simulation end   time as obtained from the masterComponent
    if (masterComponent->unit.component->onThisRank)
    {
        (masterComponent->unit.component->dllGetStartTime) (&masterComponent->tStart);
        (masterComponent->unit.component->dllGetEndTime) (&masterComponent->tEnd);
    }

    // make sure that all threads know about it ...
    if (masterComponent->unit.component->numProcesses < numranks) {
        double dbl2_buf[2];
        int ierr;

        if (masterComponent->unit.component->processes[0] == my_rank) {
            dbl2_buf[0] = masterComponent->tStart;
            dbl2_buf[1] = masterComponent->tEnd;
        }
        else {
            dbl2_buf[0] = 0.0;
            dbl2_buf[1] = 0.0;
        }
        ierr = MPI_Bcast(dbl2_buf, 2, MPI_DOUBLE, masterComponent->unit.component->processes[0], MPI_COMM_WORLD);

        masterComponent->tStart = dbl2_buf[0];
        masterComponent->tEnd = dbl2_buf[1];
    }

    masterComponent->tNext = min(*currentTime + tStep, masterComponent->tEnd);
    if (*currentTime == masterComponent->tStart) {
        // Set the currentTime and nextTime in all other components, relative to currentTime
        for (int i = 0; i < cb->numSubBlocks; i++) {
            if (i != cb->masterSubBlockId) {
                // subBlock.tCur:
                //     componentType = WAVE:
                //         Assumption: Reference date is equal to masterComponent's reference date
                //         => : tCur = 0.0
                //     componentType = RTC:
                //         tCur is not used (update is always called with argument -1)
                cb->subBlocks[i].tCur = 0.0;
                //
                // subBlock.tNext:
                //     tNext must be tStart, also when tStart=0,
                //     to distinguish "start at begin of simulatione" and "after one time step"
                for (int j = 0; j < cb->subBlocks[i].numSubBlocks; j++) {  // look for the WAVE component (wave-library)
                    if (j != cb->masterSubBlockId && cb->subBlocks[i].subBlocks[j].unit.component != nullptr && cb->subBlocks[i].subBlocks[j].unit.component->type == COMP_TYPE_WAVE) {
                        cb->subBlocks[i].tNext = *currentTime + cb->subBlocks[i].tStart;
                        cb->subBlocks[i].tEnd = *currentTime + cb->subBlocks[i].tEnd;
                    }
                    else {
                        cb->subBlocks[i].tNext = cb->subBlocks[i].tStart;
                        cb->subBlocks[i].tEnd = cb->subBlocks[i].tEnd;
                    }
                }
            }
        }
    }
    //
    // TIME LOOP
    //
    while (*currentTime < masterComponent->tNext) {
        //
        // define tStep,
        // tStep is the minimum allowed time step over all followers
        // Start with maximum value defined by masterComponent
        double tStep = masterComponent->tNext - *currentTime;
        log->Write(INFO, my_rank, "TIME LOOP %15.5f -- %15.5f -- %15.5f", tStep, masterComponent->tNext, *currentTime);
        for (int i = 0; i < cb->numSubBlocks; i++) {
            if (i != cb->masterSubBlockId) {
                // follower.tNext is the next point in time that this follower must be executed
                // For this follower, the allowed timestep is "follower.tNext - currentTime"
                double tStepFollower = cb->subBlocks[i].tNext - *currentTime;
                if (tStepFollower <= 0.0) {
                    // This follower is already active
                    if (i < cb->masterSubBlockId) {
                        // Before MasterComponent:
                        // Check the step size instead of tNext
                        tStep = min(tStep, cb->subBlocks[i].tStep);
                    }
                    else {
                        // After MasterComponent:
                        // When the MasterComponent is going to run, it will increase currentTime
                        // This does not match with executing this follower at the current time
                        throw Exception(true, Exception::ERR_INVALID_INPUT, "runParallelUpdate: Zero timestep, needed for block %d, is not possible.", i);
                    }
                }
                else {
                    // This follower is not active yet
                    tStep = min(tStep, tStepFollower);
                }
                log->Write(INFO, my_rank, "TIME LOOP %15.5f -- %15.5f -- %15.5f -- %15.5f ", tStep, masterComponent->tNext, cb->subBlocks[i].tNext, tStepFollower);
            }
        }

        //
        // Update all components in the specified order
        for (int i = 0; i < cb->numSubBlocks; i++) {
            // Sync all partitions to execute the same component
            // This ensures that all flow calculations are finished before a wave calculation is started
            if (use_mpi) {
                int ierr = MPI_Barrier(MPI_COMM_WORLD);
            }

            if (i == cb->masterSubBlockId) {
                // masterComponent
                chdir(masterComponent->unit.component->workingDir);
                log->Write(INFO, my_rank, "%10.1f:    %s.Update(%15.5f)", *currentTime, masterComponent->unit.component->name, tStep);
                timerStart(masterComponent->unit.component);
                int state = 0; // state returned by local call to component (0 if not called)
                int state0 = 0; // state returned by call to component by the first thread calling that component
                int ierr;
                if (masterComponent->unit.component->onThisRank)
                {
                    state = (masterComponent->unit.component->dllUpdate) (tStep);
                    if (masterComponent->unit.component->processes[0] == my_rank)
                    {
                        state0 = state;
                    }
                }
                if (masterComponent->unit.component->numProcesses < numranks)
                {
                    ierr = MPI_Bcast(&state0, 4, MPI_INT, masterComponent->unit.component->processes[0], MPI_COMM_WORLD);
                }
                if (state0 != 0 || state != 0)
                {
                    stringstream ss, curTime;
                    ss << state;
                    curTime << *currentTime;
                    std::string componentName = cb->unit.component->name;
                    std::string message = "#### ERROR: dimr update ABORT,: " + componentName + " update failed, with return value " + ss.str() + " at time : " + curTime.str() + "\n";
                    printf(message.c_str());
                    log->Write(FATAL, my_rank, message.c_str(), configfile);

                    throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
                }
                timerEnd(masterComponent->unit.component);
                *currentTime = *currentTime + tStep;
            }
            else {
                // CT_STARTGROUP
                if (*currentTime >= cb->subBlocks[i].tNext) {
                    // Yes, it's time to execute all childBlocks of this subBlock
                    for (int j = 0; j < cb->subBlocks[i].numSubBlocks; j++) {
                        if (cb->subBlocks[i].subBlocks[j].type == CT_START) {
                            // Component
                            dimr_component* thisComponent = cb->subBlocks[i].subBlocks[j].unit.component;
                            if (!thisComponent->onThisRank) {
                                continue;
                            }
                            //
                            // tUpdate is the timeInterval since the last time this component was executed
                            // This is not the overall tStep!
                            double tUpdate;
                            double subblock_time = cb->subBlocks[i].tCur; // Shortcut to keep the code readable
                            tUpdate = *currentTime - masterComponent->tStart - subblock_time;
                            log->Write(INFO, my_rank, "Parallel Startgroup: Current time: %15.5f -- %15.5f", *currentTime, subblock_time);
                            // Hack: Always call RTCTools.Update with argument -1.0
                            if (cb->subBlocks[i].subBlocks[j].unit.component->type == COMP_TYPE_RTC) {
                                tUpdate = -1.0;
                            }
                            // Hack: Wanda: The first time that WANDA.update(tUpdate) is called, tUpdate must be 0.0
                            //              Otherwise, WANDA will hang in case tUpdate is very big (FLOW start time big, related to ref time)
                            //              It doesn't matter for the WANDA results; with this coupling, WANDA doesn't have a time knowledge at all
                            if (cb->subBlocks[i].subBlocks[j].unit.component->type == COMP_TYPE_WANDA) {
                                if (subblock_time == 0.0) {
                                    tUpdate = 0.0;
                                }
                            }
                            if (cb->subBlocks[i].subBlocks[j].unit.component->type == COMP_TYPE_WAVE) {
                                // dwaves is using the time-frame of the master coponent
                                tUpdate += masterComponent->tStart;
                            }
                            // Update
                            chdir(thisComponent->workingDir);
                            log->Write(INFO, my_rank, "%10.1f:    %s.Update(%10.1f)", *currentTime, thisComponent->name, tUpdate);
                            timerStart(thisComponent);
                            int state = (thisComponent->dllUpdate) (tUpdate);
                            if (state != 0)
                            {
                                stringstream ss, ss_nr;
                                ss << *currentTime;
                                ss_nr << state;
                                string message = "Could not update the component " + std::string(thisComponent->name) + " at time " + ss.str() + " because of errnr : " + ss_nr.str() + "\n";
                                printf(message.c_str());
                                log->Write(FATAL, my_rank, message.c_str(), configfile);

                                throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
                            }
                            timerEnd(thisComponent);
                        }
                        else
                        {
                            // Coupler
                            dimr_coupler* thisCoupler = cb->subBlocks[i].subBlocks[j].unit.coupler;
                            double* transferValuePtr;
                            //log->Write(DEBUG, my_rank, "%10.1f:    %s.communicate (%d)", *currentTime, thisCoupler->name, timeIndexCounter);

                            // log netcdf time variable
                            int timeIndexCounter = static_cast<int>(floor((*currentTime - cb->subBlocks[cb->masterSubBlockId].tStart) / tStep));
                            log->Write(DEBUG, my_rank, "%10.1f:    %s.communicate (%d -- %15.5f, %15.5f)", *currentTime, thisCoupler->name, timeIndexCounter, cb->subBlocks[cb->masterSubBlockId].tStart, tStep);
                            if (thisCoupler->logger != NULL) {
                                string fileName = thisCoupler->logger->GetLoggerFilename(dimrWorkingDirectory, dirSeparator);

                                int ncid = ncfiles[fileName];
                                size_t index[] = { static_cast<size_t>(timeIndexCounter) };
                                nc_put_var1_double(ncid, thisCoupler->logger->netcdfReferences->timeVar, index, currentTime);
                            }

                            for (int k = 0; k < thisCoupler->numItems; k++) {
                                log->Write(ALL, my_rank, "    %s -> %s (%d)", thisCoupler->items[k].sourceName, thisCoupler->items[k].targetName, timeIndexCounter);

                                // Getting and Setting of data is split to enable
                                // transferring data, possibly inbetween different partitions
                                // TODO: This does not work for arrays (yet), only scalar double
                                //
                                // Getting data:
                                // "transfer" is the value on this rank. Set it to a very big negative value
                                double transfer = -999000.0;

                                // addresses eventually updated
                                // only when the source component is on this rank
                                // Wanda will drop the value in "transfer" on calling dllGetVar
                                if (thisCoupler->sourceComponent->onThisRank) {
                                    getAddress(thisCoupler->items[k].sourceName, thisCoupler->sourceComponent->type, thisCoupler->sourceComponent->dllGetVar, &(thisCoupler->items[k].sourceVarPtr),
                                        thisCoupler->sourceComponent->processes, thisCoupler->sourceComponent->numProcesses, transfer);
                                }
                                // Use sourceVarPtr to fill "transfer" and sent it to all ranks,
                                // resulting in transferValuePtr pointing to the correct value on all ranks
                                transferValuePtr = send(thisCoupler->items[k].sourceName, thisCoupler->sourceComponent->type, thisCoupler->items[k].sourceVarPtr,
                                    thisCoupler->sourceComponent->processes, thisCoupler->sourceComponent->numProcesses, &transfer);

                                // Optional TODO: assuming one source(partition):
                                //    if there is only one target (partition), and all partitions can act as source partition:
                                //        choose the target partition to act as source partition
                                //        no MPI_Bcast needed
                                //
                                if (thisCoupler->itemTypes[k] == ITEM_TYPE_SCALAR)
                                {
                                    receive(thisCoupler->items[k].targetName,
                                         thisCoupler->targetComponent->type,
                                         thisCoupler->targetComponent->dllSetVar,
                                         thisCoupler->targetComponent->dllGetVar,
                                         thisCoupler->items[k].targetVarPtr,
                                         thisCoupler->targetComponent->processes,
                                         thisCoupler->targetComponent->numProcesses,
                                         thisCoupler->items[k].targetProcess,
                                         transferValuePtr);
                                } else {
                                    receive_ptr (thisCoupler->items[k].targetName,
                                         thisCoupler->items[k].sourceName,
                                         thisCoupler->targetComponent->type,
                                         thisCoupler->targetComponent->dllSetVar,
                                         thisCoupler->targetComponent->dllGetVar,
                                         thisCoupler->sourceComponent->dllGetVarShape,
                                         thisCoupler->items[k].targetVarPtr,
                                         thisCoupler->targetComponent->processes,
                                         thisCoupler->targetComponent->numProcesses,
                                         thisCoupler->items[k].targetProcess,
                                         thisCoupler->items[k].sourceVarPtr);
                                }

                                if (thisCoupler->logger != NULL && my_rank == 0)
                                {
                                    string fileName = thisCoupler->logger->GetLoggerFilename(this->dimrWorkingDirectory, this->dirSeparator);

                                    int ncid = ncfiles[fileName];
                                    size_t indices[] = { static_cast<size_t>(timeIndexCounter), 0 };
                                    int status = nc_put_var1_double(ncid, thisCoupler->logger->netcdfReferences->item_variables[k], indices, transferValuePtr);
                                    if (status != NC_NOERR)
                                        throw Exception(true, Exception::ERR_OS, "Could not write value at index (%i, 0).", timeIndexCounter);
                                }

                                // Force update of the pointers for ITEM_TYPE_PTR
                                // Needed because of the multi realloc calls
                                if (thisCoupler->itemTypes[k] == ITEM_TYPE_PTR)
                                {
                                    thisCoupler->items[k].targetVarPtr = NULL;
                                    thisCoupler->items[k].sourceVarPtr = NULL;
                                }
                            }
                        }
                    }
                    // All child blocks are executed. Update tCur and define the next time step that this block has to be executed.
                    for (int j = 0; j < cb->subBlocks[i].numSubBlocks; j++) {  // look for the WAVE component (wave-library)
                        if (j != cb->masterSubBlockId && cb->subBlocks[i].subBlocks[j].unit.component != nullptr && cb->subBlocks[i].subBlocks[j].unit.component->type == COMP_TYPE_WAVE) {
                            cb->subBlocks[i].tCur = *currentTime;
                            log->Write(INFO, my_rank, "Parallel Child: Current time: %15.5f -- %d", *currentTime, i);
                        }
                        else {
                            cb->subBlocks[i].tCur = *currentTime - masterComponent->tStart;
                            log->Write(INFO, my_rank, "Parallel Child 2: Current time: %15.5f -- %d", *currentTime, i);
                        }
                    }
                    cb->subBlocks[i].tNext = cb->subBlocks[i].tNext + cb->subBlocks[i].tStep;
                    vector<double>& subblock_timearray = *(cb->subBlocks[i].computeTimes); // Shortcut to keep the code readable
                    if (cb->subBlocks[i].tNext > cb->subBlocks[i].tEnd)
                        // This subBlock does not have to be executed anymore
                        // Force this by giving it a nextTime > simulationEndTime
                        cb->subBlocks[i].tNext = 2.0 * masterComponent->tEnd;
                    else if (cb->subBlocks[i].computeTimesCurrent > 0) {
                        // Computation times specified in a time series:
                        // Use the next element from the computeTimes array
                        cb->subBlocks[i].computeTimesCurrent++;
                        int subblock_timeindex = cb->subBlocks[i].computeTimesCurrent;  // Shortcut to keep the code readable
                        // If there are no valid timepoints in array computeTimes anymore:
                        //     Ensure that tStep is big enough to cover the interval from currentTime to tEnd
                        if (subblock_timeindex >= subblock_timearray.size())
                            cb->subBlocks[i].tStep = 2.0 * (masterComponent->tEnd - *currentTime);
                        else {
                            // Substract the previous computeTimes-entry to obtain the new tStep
                            cb->subBlocks[i].tStep = subblock_timearray[subblock_timeindex] - subblock_timearray[subblock_timeindex - 1];
                        }
                    }
                }
            }
        }
    }
}

//------------------------------------------------------------------------------
// set "value" in the component target location
void Dimr::receive(const char* name,
    int          compType,
    BMI_SETVAR   dllSetVar,
    BMI_GETVAR   dllGetVar,
    double* targetVarPtr,
    int* processes,
    int          nProc,
    int          targetProcess,
    const void* transferValuePtr) {
    // Assumption: transferValuePtr points to a (scalar) double
    // TODO: allow more types of pointers
    //
    // NaN check:
    if (*(double*)(transferValuePtr) == *(double*)(transferValuePtr)) {
        for (int m = 0; m < nProc; m++) {
            if (my_rank == processes[m]) {
                log->Write(ALL, my_rank, "Dimr::receive(%s) %20.10f", name, *(double*)(transferValuePtr));
                if (compType == COMP_TYPE_RTC
                    // || compType == COMP_TYPE_RR // SOBEK-51004: RR must use explicit set_var as long as get_var is not properly implemented for some variables.
                    || compType == COMP_TYPE_FLOW1D
                    || compType == COMP_TYPE_FLOW1D2D
                    || compType == COMP_TYPE_ZSF
                    || compType == COMP_TYPE_WANDA) {
                    if (dllSetVar == NULL) {
                        throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "ABORT: Dimr::receive: set_var function not defined while processing %s", name);
                    }
                    (dllSetVar)(name, (const void*)transferValuePtr);
                    if (compType == COMP_TYPE_RTC || compType == COMP_TYPE_ZSF) {
                        // target = rtc
                        // SetVar(name, value) sets variable named "name" to "value" at the current time (t = n)
                        // But, in case of IMPLICIT method, this should be the next time (t = n + 1)
                        // Clean solution: do not use IMPLICIT method in RTCTools (To Do for Stef Hummel)
                        // Shortcut hack:
                        // 1. Set value at current time (t=n)
                        // AND 2. Set also value at next time (t=n+1),
                        // by adding a * at the end of the targetName. This is a signal to RTCTools that this
                        // value belongs to t=n+1
                        // This "shortcut hack" is identical to what currently is used in Delta Shell via the
                        // OpenMI interface with RTCTools
                        char* targetName = new char[strlen(name) + 2];
                        sprintf(targetName, "%s*\0", name);
                        (dllSetVar)(targetName, (const void*)transferValuePtr);
                        delete[] targetName;
                    }
                }
                else {
                    // target is a component that uses direct pointer access to the actual variable
                    // When doing a dllSetVar, targetVarPtr is not defined. First do a "getAddress" to get it defined
                    // NOTE: transferValuePtr contains the value calculated by RTCTools: it has been already communicated
                    // to all processes by a previous send call.

                    if (targetVarPtr == NULL)
                    {
                        double transfer = -999000.0;
                        //here we get the address (e.g. weir levels)
                        getAddress(name, compType, dllGetVar, &targetVarPtr, processes, nProc, transfer);
                    }

                    //  }
                    // Know we know the location, we need to set it in targetVarPtr
                    //here we do the check
                    if (targetVarPtr == NULL)
                    {
                        if (targetProcess == -1 || targetProcess == my_rank)
                        {
                            // targetProcess=-1: no process can accept this item
                            // targetProcess=my_rank: this process is registered to be able to accept this item but something goes wrong
                            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "ABORT: Dimr::receive: get_var function not defined while processing %s", name);
                        }
                    }
                    else
                    {
                        // Here write the RTC value in FM (direct access to memory using a pointer).
                        // Here we already know targetVarPtr is not null.
                        *(targetVarPtr) = *(double*)transferValuePtr;
                    }
                }
            }
        }
    }
}

//------------------------------------------------------------------------------
// set "value" in the component target location
void Dimr::receive_ptr(const char * name,
        const char * sourceName,
        int          compType,
        BMI_SETVAR   dllSetVar,
        BMI_GETVAR   dllGetVar,
        BMI_GETVARSHAPE dllGetVarShape,
        double     * targetVarPtr,
        int        * processes,
        int          nProc,
        int          targetProcess,
        double     * sourceVarPtr) {

        // First: call GetVarShape("",shapeArr)
        int shape[6];
        (dllGetVarShape)(sourceName, shape);
        // Second: call setvar(name_shape, shape)
        char nameShape[100];
        strcpy(nameShape, name);
        strcat(nameShape, "_shape");
        (dllSetVar)(nameShape, shape);
        // Finally: call setvar(name, pointer)
        (dllSetVar)(name, (void*)sourceVarPtr);

        // target is a component that uses direct pointer access to the actual variable
        // When doing a dllSetVar, targetVarPtr is not defined yet. First do a "getAddress" to get it defined

        if (targetVarPtr == NULL)
        {
                double transfer = -999000.0;
                //here we get the address (e.g. weir levels)
                getAddress(name, compType, dllGetVar, &targetVarPtr, processes, nProc, transfer);
        }

        // Now targetVarPtr must be defined
        if (targetVarPtr == NULL)
        {
                if (targetProcess == -1 || targetProcess == my_rank)
                {
                        // targetProcess=-1: no process can accept this item
                        // targetProcess=my_rank: this process is registered to be able to accept this item but something goes wrong
                        throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "ABORT: Dimr::receive: get_var function not defined while processing %s", name);
                }
        }
}

//------------------------------------------------------------------------------
// This function gets the address from the components.
// Does not do any transfer of the values stored in sourceVarPtr.

void Dimr::getAddress(
    const char* name,
    int          compType,
    BMI_GETVAR   dllGetVar,
    double** sourceVarPtr,
    int* processes,
    int          nProc,
    double& transfer)
{
    log->Write(ALL, my_rank, "Dimr::getAddress (%s)", name);

    // These components only return a new pointer to a copy of the double value, so call it each time.
    std::set<int> second_component_set = { COMP_TYPE_DEFAULT_BMI, COMP_TYPE_RTC, COMP_TYPE_ZSF, COMP_TYPE_FLOW1D, COMP_TYPE_FLOW1D2D, COMP_TYPE_FM, COMP_TYPE_DELWAQ };

    // The order is important: first catch the Wanda case:
    // Otherwise the "else if" part might be executed When "*sourceVarPtr==NULL" and "compType==COMP_TYPE_WANDA"
    if (compType == COMP_TYPE_WANDA) {
        if (dllGetVar == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "ABORT: get_var function not defined while processing %s", name);
        }
        // Wanda does not use pointers to internal structures:
        // - Use the DIMR-transfer array
        // - Note the added * in the dllGetVar call, when comparing with the dllGetVar call below
        *sourceVarPtr = &transfer;
        (dllGetVar)(name, (void**)(*sourceVarPtr));
    }
    else if (second_component_set.find(compType) != second_component_set.end() || *sourceVarPtr == NULL) {
                // sourceVarPtr=NULL: getVar not yet called for this parameter, probably because "send" is being called
                //                    via the toplevel "get_var"
                if (dllGetVar == NULL) {
                    throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "ABORT: get_var function not defined while processing %s", name);
                }
                log->Write(ALL, my_rank, "Dimr::getAddress -- calling");
                (dllGetVar)(name, (void**)(sourceVarPtr));
    }
    else
    {
        // Other components already have direct pointer access to the actual variable.
    }
}

//------------------------------------------------------------------------------
// This function sends the sourceVarPtr value to all processes
double* Dimr::send(
    const char* name,
    int          compType,
    double* sourceVarPtr,
    int* processes,
    int          nProc,
    double* transfer)
{
    log->Write(ALL, my_rank, "Dimr::send (%s)", name);
    if (sourceVarPtr != NULL && compType != COMP_TYPE_WANDA)
    {
        *transfer = *sourceVarPtr;
    }

    // Do not call MPI_Allreduce when the number of partitions is 1.
    if (numranks > 1)
    {
        // NOTE: here the transfer array has a defined value only at position==my_rank and if *sourceVarPtr != NULL.
        // We perform a reduction operation to collect the maximum values at each position and afterwards take the maximum of all values.
        // We could also use only one double instead of a transfer array and perform the reduction on a single scalar (so avoiding the loop below)
        int ierr = MPI_Allreduce(transfer, &(transferValue), 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);

    }
    else
    {
        transferValue = *transfer;
    }
    return &(transferValue);
}


//------------------------------------------------------------------------------
void Dimr::runParallelFinish(dimr_control_block* cb) {
    if (use_mpi) {
        int ierr = MPI_Barrier(MPI_COMM_WORLD);
    }

    for (int i = 0; i < cb->numSubBlocks; i++) {
        if (cb->subBlocks[i].type == CT_START) {
            if (!cb->subBlocks[i].unit.component->onThisRank) {
                continue;
            }
            chdir(cb->subBlocks[i].unit.component->workingDir);
            log->Write(INFO, my_rank, "    %s.Finalize()", cb->subBlocks[i].unit.component->name);
            timerStart(cb->subBlocks[i].unit.component);
            int state = (cb->subBlocks[i].unit.component->dllFinalize) ();
            if (state != 0)
            {
                stringstream ss;
                ss << state;
                std::string componentName = cb->subBlocks[i].unit.component->name;
                std::string message = "#### ERROR: dimr finalize ABORT,: " + componentName + " finalize failed, with return value " + ss.str() + " \n";
                printf(message.c_str());
                log->Write(FATAL, my_rank, message.c_str(), configfile);

                throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
            }
            timerEnd(cb->subBlocks[i].unit.component);

            if (use_mpi && cb->subBlocks[i].unit.component->mpiComm != NULL) {
                MPI_Group mpiGroupComp;
                int ierr = MPI_Comm_group(cb->subBlocks[i].unit.component->mpiComm, &mpiGroupComp);
                ierr = MPI_Group_free(&mpiGroupComp);
                // Is this really needed? In dflowfm the communicator is already freed in petsc finalize is solver 6 is used and this line causes a runtime error at the end of a dimr run
                //ierr = MPI_Comm_free(&cb->subBlocks[i].unit.component->mpiComm);
            }
        }
        else {
            // CT_STARTGROUP
            for (int j = 0; j < cb->subBlocks[i].numSubBlocks; j++) {
                if (cb->subBlocks[i].subBlocks[j].type == CT_START) {
                    if (!cb->subBlocks[i].subBlocks[j].unit.component->onThisRank) {
                        continue;
                    }
                    chdir(cb->subBlocks[i].subBlocks[j].unit.component->workingDir);
                    log->Write(INFO, my_rank, "    %s.Finalize()", cb->subBlocks[i].subBlocks[j].unit.component->name);
                    timerStart(cb->subBlocks[i].subBlocks[j].unit.component);
                    int state = (cb->subBlocks[i].subBlocks[j].unit.component->dllFinalize) ();
                    if (state != 0)
                    {
                        stringstream ss;
                        ss << state;
                        std::string componentName = cb->subBlocks[i].unit.component->name;
                        std::string message = "#### ERROR: dimr finalize ABORT,: " + componentName + " finalize failed, with return value " + ss.str() + " \n";
                        printf(message.c_str());
                        log->Write(FATAL, my_rank, message.c_str(), configfile);

                        throw Exception(true, Exception::ERR_UNKNOWN, message.c_str());
                    }
                    timerEnd(cb->subBlocks[i].subBlocks[j].unit.component);
                }
                else { //coupler
                    dimr_coupler* thisCoupler = cb->subBlocks[i].subBlocks[j].unit.coupler;
                    if (thisCoupler->logger != NULL) {
                        string fileName = thisCoupler->logger->GetLoggerFilename(dimrWorkingDirectory, dirSeparator);
                        int ncid = ncfiles[fileName];
                        if (ncid >= 0) {
                            // todo: what if the computation crashes - can we read the file?
                            nc_close(ncid);
                        }
                    }
                }
            }
        }
    }
}

//------------------------------------------------------------------------------
void Dimr::scanConfigFile(void) {

    XmlTree* rootXml = static_cast<XmlTree*>(config->Lookup("/dimrConfig"));
    if (rootXml == NULL)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "Configuration file \"%s\" does not have a <dimrConfig> root element", configfile);
    XmlTree* fileversion = rootXml->Lookup("documentation/fileVersion");
    if (fileversion == NULL)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "Configuration file \"%s\" does not have a deltaresHydro documentation->fileVersion element", configfile);

    // Check version number
    const char* versionnr = fileversion->charData;
    float versionnumber;
    int intRead = sscanf(versionnr, "%f", &versionnumber);
    if (intRead != 1)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "Configuration file \"%s\" does not have a version number", configfile);
    if ((int)floor(versionnumber) != 1)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "Configuration file \"%s\": Version number (%3.2f) must have main version 2", configfile, versionnumber);

    XmlTree* controlXml = rootXml->Lookup("control");
    if (controlXml == NULL)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "Configuration file \"%s\" does not have a deltaresHydro control element", configfile);
    // Allocate the control structure and check its size
    control = (dimr_control_block*)malloc(sizeof(dimr_control_block));
    control->numSubBlocks = 0;
    control->subBlocks = NULL;
    control->masterSubBlockId = -1;
    // First scan the global settings
    scanGlobalSettings(rootXml);
    // Then scan the config file for all components and couplers (= units)
    scanUnits(rootXml);
    // Then scan the control part
    // References are added to the list of components/couplers
    scanControl(controlXml, control);
}



//------------------------------------------------------------------------------
void Dimr::scanGlobalSettings(XmlTree* rootXml) {
    // Init
    nc_mode = 0;
    // Scan
    XmlTree* globalSettings = rootXml->Lookup("global_settings");
    if (globalSettings == NULL)
        return;
    XmlTree* loggerNcFormat = globalSettings->Lookup("logger_ncFormat");
    if (loggerNcFormat != NULL) {
        int intRead = sscanf(loggerNcFormat->charData, "%d", &(nc_mode));
        if (intRead != 1)
            throw Exception(true, Exception::ERR_INVALID_INPUT, "logger_ncFormat must contain the value 3 or 4");
        if (nc_mode == 3) {
            nc_mode = NC_CLASSIC_MODEL;
        }
        else if (nc_mode == 4) {
            nc_mode = NC_NETCDF4;
        }
        else {
            nc_mode = NC_CLOBBER;
        }

    }
}


//------------------------------------------------------------------------------
void Dimr::scanUnits(XmlTree* rootXml) {
    // Init
    componentsList.numComponents = 0;
    componentsList.components = NULL;
    couplersList.numCouplers = 0;
    couplersList.couplers = NULL;
    // Scan
    for (int i = 0; i < rootXml->children.size(); i++) {
        if (strcmp(rootXml->children[i]->name, "component") == 0) {
            componentsList.numComponents++;
            if (componentsList.components == NULL) {
                componentsList.components = (dimr_component*)malloc(componentsList.numComponents * sizeof(dimr_component));
            }
            else {
                componentsList.components = (dimr_component*)realloc(componentsList.components,
                    componentsList.numComponents * sizeof(dimr_component));
                if (componentsList.components == NULL) {
                    throw Exception(true, Exception::ERR_INVALID_INPUT, "Allocation error in scanUnits (component)");
                }
            }
            scanComponent(rootXml->children[i], &(componentsList.components[componentsList.numComponents - 1]));
        }
        if (strcmp(rootXml->children[i]->name, "coupler") == 0) {
            couplersList.numCouplers++;
            if (couplersList.couplers == NULL) {
                couplersList.couplers = (dimr_coupler*)malloc(couplersList.numCouplers * sizeof(dimr_coupler));
            }
            else {
                couplersList.couplers = (dimr_coupler*)realloc(couplersList.couplers, couplersList.numCouplers * sizeof(dimr_coupler));
                if (couplersList.couplers == NULL) {
                    throw Exception(true, Exception::ERR_INVALID_INPUT, "Allocation error in scanUnits (coupler)");
                }
            }
            scanCoupler(rootXml->children[i], &(couplersList.couplers[couplersList.numCouplers - 1]));
        }
    }
}



//------------------------------------------------------------------------------
void Dimr::scanComponent(XmlTree* xmlComponent, dimr_component* newComp) {
    // Needed for path handling
    char* curPath = new char[MAXSTRING];
    if (!getcwd(curPath, MAXSTRING))
        throw Exception(true, Exception::ERR_OS, "ERROR obtaining the current working directory (scan)");
    //
    //
    newComp->name = xmlComponent->GetAttrib("name");
    // Element library
    XmlTree* libraryElement = xmlComponent->Lookup("library");
    if (libraryElement == NULL)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "Component \"%s\" does not contain a library element", newComp->name);
    newComp->library = libraryElement->charData;
    int libLen = strlen(newComp->library);
    char* libNameLowercase = new char[libLen + 1];
    strncpy(libNameLowercase, newComp->library, libLen);
    libNameLowercase[libLen] = '\0';
    for (int i = 0; i < libLen; i++) {
        libNameLowercase[i] = (tolower(libNameLowercase[i]));
    }
    if (strstr(libNameLowercase, "fbc") != NULL) {
        newComp->type = COMP_TYPE_RTC;
    }
    else if (strstr(libNameLowercase, "wave") != NULL) {
        newComp->type = COMP_TYPE_WAVE;
    }
    else if (strstr(libNameLowercase, "fm") != NULL) {
        newComp->type = COMP_TYPE_FM;
    }
    else if (strstr(libNameLowercase, "cf_dll") != NULL) {
        newComp->type = COMP_TYPE_FLOW1D;
    }
    else if (strstr(libNameLowercase, "rr_dll") != NULL) {
        newComp->type = COMP_TYPE_RR;
    }
    else if (strstr(libNameLowercase, "wanda") != NULL) {
        newComp->type = COMP_TYPE_WANDA;
    }
    else if (strstr(libNameLowercase, "flow2d3d") != NULL) {
        newComp->type = COMP_TYPE_FLOW2D3D;
    }
    else if (strstr(libNameLowercase, "flow1d2d") != NULL) {
        newComp->type = COMP_TYPE_FLOW1D2D;
    }
    else if (strstr(libNameLowercase, "delwaq") != NULL) {
        newComp->type = COMP_TYPE_DELWAQ;
    }
    else if (strstr(libNameLowercase, "cosumo_bmi") != NULL) {
        newComp->type = COMP_TYPE_COSUMO_BMI;
    }
    else if (strstr(libNameLowercase, "zsf") != NULL) {
        newComp->type = COMP_TYPE_ZSF;
    }
    else if (strstr(libNameLowercase, "dimr_testcomponent") != NULL) {
        newComp->type = COMP_TYPE_TEST;
    }
    else
    {
        newComp->type = COMP_TYPE_DEFAULT_BMI;
        log->Write(ALL, my_rank, "INFO: \"<process>\" Type for component \"%s\" is set to default value 0", newComp->name);
    }
    delete[] libNameLowercase;

    // Element process (optional)
    XmlTree* processElement = xmlComponent->Lookup("process");
    if (processElement != NULL) {
        // Store process rank numbers in component's processes array.
        char_to_ints(processElement->charData, &(newComp->processes), &(newComp->numProcesses));

        // Check whether this process' rank is also in components configured processes array.
        newComp->onThisRank = false;         // Not found (yet): only active on other ranks.
        for (int i = 0; i < newComp->numProcesses; i++) {
            if (newComp->processes[i] >= numranks) {
                throw Exception(true, Exception::ERR_INVALID_INPUT, "Component \"%s\" configured for process #%d, but max running MPI rank is only %d.",
                    newComp->name, newComp->processes[i], numranks - 1);
            }
            else  if (newComp->processes[i] == my_rank) {
                newComp->onThisRank = true;  // Found:     active.
            }
        }

    }
    else {
        // No <process> specified, default: only run on rank #0.
        log->Write(ALL, my_rank, "INFO: \"<process>\" not specified for component \"%s\". Assuming it only runs on rank #0.", newComp->name);
        newComp->numProcesses = 1;
        char* defaultProc = "0";
        char_to_ints(defaultProc, &(newComp->processes), &(newComp->numProcesses));

        newComp->onThisRank = (my_rank == 0);
    }

    // parameter (optional)
    int nparameter = xmlComponent->Lookup("parameter", 0, newComp->parameters);

    // setting (optional)
    int nsetting = xmlComponent->Lookup("setting", 0, newComp->settings);

    // Element mpiCommunicator (optional)
    XmlTree* commElement = xmlComponent->Lookup("mpiCommunicator");
    if (commElement != NULL) {
        // Store communicator var name in component.
        newComp->mpiCommVar = commElement->charData;
    }

    // Element inputFile (optional?)
    XmlTree* inputFileElement = xmlComponent->Lookup("inputFile");
    if (inputFileElement == NULL) {
        log->Write(INFO, my_rank, "WARNING: No inputFile specified for component %s.", newComp->name);
    }
    else {
        newComp->inputFile = inputFileElement->charData;
    }
    // Element workingDir
    XmlTree* workingDirElement = xmlComponent->Lookup("workingDir");
    if (workingDirElement == NULL) {
        newComp->workingDir = (char*)&curPath;
        log->Write(INFO, my_rank, "WARNING: No workingDir specified for component %s.", newComp->name);
        log->Write(INFO, my_rank, "         workingDir is set to %s", newComp->workingDir);
    }
    else {
        newComp->workingDir = workingDirElement->charData;
    }
    // Is workingDir a valid relative path?
    char* combinedPath = new char[MAXSTRING];
    sprintf(combinedPath, "%s%s%s", curPath, dirSeparator, newComp->workingDir);
    if (chdir(combinedPath)) {
        // CombinedPath is not correct. May be just workingDir?
        delete[] combinedPath;
        // Is workingDir a valid absolute path?
        if (chdir(newComp->workingDir))
        {
            throw Exception(true, Exception::ERR_INVALID_INPUT, "Component \"%s\" has an invalid workingDir \"%s\"", newComp->name, newComp->workingDir);
        }
    }
    else {
        // newComp->workingDir was a pointer to workingDirElement->charData; now it will point to the new combinedPath
        newComp->workingDir = combinedPath;

    }
    chdir(curPath);

    // Hack for RTC-Tools:
    // inputFile is an input directory
    // "." means workingDir
    if (strcmp(newComp->inputFile, ".") == 0) {
        newComp->inputFile = newComp->workingDir;
    }
    delete[] curPath;
}



//------------------------------------------------------------------------------
void Dimr::scanCoupler(XmlTree* xmlCoupler, dimr_coupler* newCoup) {
    newCoup->name = xmlCoupler->GetAttrib("name");
    // Element sourceComponent
    XmlTree* sourceComponent = xmlCoupler->Lookup("sourceComponent");
    if (sourceComponent == NULL)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "The coupler \"%s\" does not contain a sourceComponent element", newCoup->name);
    newCoup->sourceComponentName = sourceComponent->charData;
    // Add reference to the actual component acting as source
    newCoup->sourceComponent = getComponent(newCoup->sourceComponentName);
    // Element targetComponent
    XmlTree* targetComponent = xmlCoupler->Lookup("targetComponent");
    if (targetComponent == NULL)
        throw Exception(true, Exception::ERR_INVALID_INPUT, "The coupler \"%s\" does not contain a targetComponent element", newCoup->name);
    newCoup->targetComponentName = targetComponent->charData;
    // Add reference to the actual component acting as target
    newCoup->targetComponent = getComponent(newCoup->targetComponentName);
    // Items
    newCoup->numItems = 0;
    newCoup->items = NULL;
    for (int j = 0; j < xmlCoupler->children.size(); j++)
    {
        if (strcmp(xmlCoupler->children[j]->name, "item") == 0) {
            // Create the item
            newCoup->numItems++;
            if (newCoup->items == NULL)
            {
                newCoup->items = (dimr_couple_item*)malloc(newCoup->numItems * sizeof(dimr_couple_item));
                newCoup->itemTypes = (unsigned int*)malloc(newCoup->numItems * sizeof(unsigned int));
            }
            else
            {
                newCoup->items = (dimr_couple_item*)realloc(newCoup->items, newCoup->numItems * sizeof(dimr_couple_item));
                newCoup->itemTypes = (unsigned int*)realloc(newCoup->itemTypes, newCoup->numItems * sizeof(unsigned int));
                if (newCoup->items == NULL || newCoup->itemTypes == NULL)
                {
                    throw Exception(true, Exception::ERR_INVALID_INPUT, "Allocation error in scanUnits (couple unit)");
                }
            }
            if (xmlCoupler->children[j]->GetAttrib("type") == NULL || strcmp(xmlCoupler->children[j]->GetAttrib("type"), "pointer") != 0) {
                newCoup->itemTypes[newCoup->numItems - 1] = ITEM_TYPE_SCALAR;
            } else {
                newCoup->itemTypes[newCoup->numItems - 1] = ITEM_TYPE_PTR;
            }

            dimr_couple_item* newItem = &(newCoup->items[newCoup->numItems - 1]);

            // Read sourceName
            XmlTree* xmlSource = xmlCoupler->children[j]->Lookup("sourceName");
            if (xmlSource == NULL)
                throw Exception(true, Exception::ERR_INVALID_INPUT, "The coupler \"%s\", item %d, does not contain a sourceName element", newCoup->name, newCoup->numItems);
            newItem->sourceName = xmlSource->charData;
            if (newItem->sourceName == NULL)
                throw Exception(true, Exception::ERR_INVALID_INPUT, "Item %d of coupler \"%s\" does not contain a source::name element", newCoup->numItems, newCoup->name);

            // Read targetName
            XmlTree* xmlTarget = xmlCoupler->children[j]->Lookup("targetName");
            if (xmlTarget == NULL)
                throw Exception(true, Exception::ERR_INVALID_INPUT, "The coupler \"%s\", item %d, does not contain a targetName element", newCoup->name, newCoup->numItems);
            newItem->targetName = xmlTarget->charData;
            if (newItem->targetName == NULL)
                throw Exception(true, Exception::ERR_INVALID_INPUT, "Item %d of coupler \"%s\" does not contain a target::name element", newCoup->numItems, newCoup->name);

            // source/targetVarPtr will be set in runParallelInit
            newItem->sourceVarPtr = NULL;
            newItem->targetVarPtr = NULL;
        }
    }

    // BMILogger (optional)
    newCoup->logger = NULL;
    XmlTree* logger = xmlCoupler->Lookup("logger");
    if (logger != NULL) {
        // Store settings in coupler's logger.
        newCoup->logger = (dimr_logger*)malloc(sizeof(dimr_logger));
        newCoup->logger->netcdfReferences = (netcdf_references*)malloc(sizeof(netcdf_references));

        // Read workingDir
        newCoup->logger->workingDir = logger->GetElement("workingDir");
        if (newCoup->logger->workingDir == NULL)
            throw Exception(true, Exception::ERR_INVALID_INPUT, "The coupler \"%s\"'s logger element does not contain a workingDir element.", newCoup->name);

        // Read fileName
        newCoup->logger->outputFile = logger->GetElement("outputFile");
        if (newCoup->logger->outputFile == NULL)
            throw Exception(true, Exception::ERR_INVALID_INPUT, "The coupler \"%s\"'s logger element does not contain an outputFile element.", newCoup->name);
    }
}


//------------------------------------------------------------------------------
void Dimr::scanControl(XmlTree* controlBlockXml, dimr_control_block* controlBlock) {
    if (strcmp(controlBlockXml->name, "control") == 0) {
        controlBlock->type = CT_SEQUENTIAL;
    }
    else if (strcmp(controlBlockXml->name, "parallel") == 0) {
        controlBlock->type = CT_PARALLEL;
    }
    else if (strcmp(controlBlockXml->name, "start") == 0) {
        controlBlock->type = CT_START;
        controlBlock->unit.component = getComponent(controlBlockXml->GetAttrib("name"));
        controlBlock->unit.coupler = NULL;
    }
    else if (strcmp(controlBlockXml->name, "startGroup") == 0) {
        controlBlock->type = CT_STARTGROUP;
        XmlTree* timeElt = controlBlockXml->Lookup("time");
        if (timeElt == NULL)
            throw Exception(true, Exception::ERR_INVALID_INPUT, "The startGroup component \"%s\" does not contain a time element", controlBlockXml->name);
        // The time field either contains:
        // - tStart, tStep, tStop                             , e.g. <time>0.0 3.6e3 9.99e4</time>
        // - name of a file containing computation time points, e.g. <time>wave_computations.tim</time>
        // First check whether it's the name of a file:
        if (!readComputeTimesFile(timeElt->charData, controlBlock)) {
            // No, it's not the name of a file. Assume that it contains tStart, tStep, tStop
            int intRead = sscanf(timeElt->charData, "%lf %lf %lf", &(controlBlock->tStart), &(controlBlock->tStep), &(controlBlock->tEnd));
            if (intRead != 3)
                throw Exception(true, Exception::ERR_INVALID_INPUT, "'%s' must either contain 'tStart, tStep, tEnd' or the name of a time series file", timeElt->charData);
            // computeTimesCurrent>0 indicates a time series read from file
            controlBlock->computeTimesCurrent = -1;
        }
    }
    else if (strcmp(controlBlockXml->name, "coupler") == 0) {
        controlBlock->type = CT_COUPLER;
        controlBlock->unit.component = NULL;
        controlBlock->unit.coupler = getCoupler(controlBlockXml->GetAttrib("name"));
    }
    controlBlock->numSubBlocks = 0;
    controlBlock->subBlocks = NULL;
    controlBlock->masterSubBlockId = -1;
    for (int i = 0; i < controlBlockXml->children.size(); i++) {
        if (strcmp(controlBlockXml->children[i]->name, "parallel") == 0
            || strcmp(controlBlockXml->children[i]->name, "start") == 0
            || strcmp(controlBlockXml->children[i]->name, "startGroup") == 0
            || strcmp(controlBlockXml->children[i]->name, "coupler") == 0) {
            controlBlock->numSubBlocks++;
            if (controlBlock->subBlocks == NULL) {
                controlBlock->subBlocks = (dimr_control_block*)malloc(controlBlock->numSubBlocks * sizeof(dimr_control_block));
            }
            else {
                controlBlock->subBlocks = (dimr_control_block*)realloc(controlBlock->subBlocks, controlBlock->numSubBlocks * sizeof(dimr_control_block));
                if (controlBlock->subBlocks == NULL) {
                    throw Exception(true, Exception::ERR_INVALID_INPUT, "Allocation error in scanControl");
                }
            }
            scanControl(controlBlockXml->children[i], &(controlBlock->subBlocks[controlBlock->numSubBlocks - 1]));
        }
        fflush(stdout);
    }
}



//------------------------------------------------------------------------------
// Search for a named component in the list of components
dimr_component* Dimr::getComponent(const char* compName) {
    for (int i = 0; i < componentsList.numComponents; i++) {
        if (strcmp(componentsList.components[i].name, compName) == 0) {
            return &(componentsList.components[i]);
        }
    }
}



//------------------------------------------------------------------------------
// Search for a named coupler in the list of couplers
dimr_coupler* Dimr::getCoupler(const char* coupName) {
    for (int i = 0; i < couplersList.numCouplers; i++) {
        if (strcmp(couplersList.couplers[i].name, coupName) == 0) {
            return &(couplersList.couplers[i]);
        }
    }
}


//------------------------------------------------------------------------------
// Search for a named coupler in the list of couplers
bool Dimr::IsCouplerItemTypePTR(int couplerItem) {
    return couplerItem == ITEM_TYPE_PTR;
}


//------------------------------------------------------------------------------
void Dimr::connectLibs(void) {

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw Exception(true, Exception::ERR_OS, "ABORT: %s has not be ported to Apple Mac OS/X yet", exeName);
#endif
#ifndef _WIN32
    char* err;
#endif

    // do for all libraries
    for (int i = 0; i < componentsList.numComponents; i++) {
        if (!componentsList.components[i].onThisRank) {
            log->Write(ALL, my_rank, "Not necessary to load component library \"%s\" on this rank.", componentsList.components[i].library);
            continue;
        }

#ifndef _WIN32
        char* lib = new char[strlen(componentsList.components[i].library) + 3 + 3 + 1];
        sprintf(lib, "lib%s%s", componentsList.components[i].library, D3D_PLUGIN_EXT);
        if (strchr(componentsList.components[i].library, '/') != NULL
            || strchr(componentsList.components[i].library, '\\') != NULL
            || strchr(componentsList.components[i].library, '.') != NULL) {
            throw Exception(true, Exception::ERR_INVALID_INPUT, "Invalid component library name \"%s\"\n", lib, -1);
        }
#else
        char* lib = new char[strlen(componentsList.components[i].library) + 4 + 1];
        sprintf(lib, "%s.dll", componentsList.components[i].library);
        if (strchr(componentsList.components[i].library, '/') != NULL
            || strchr(componentsList.components[i].library, '\\') != NULL
            || strchr(componentsList.components[i].library, '.') != NULL) {
            throw Exception(true, Exception::ERR_INVALID_INPUT, "Invalid component library name \"%s\"\n", lib, -1);
        }
#endif

        log->Write(ALL, my_rank, "Loading library \"%s\"", lib);

#ifndef _WIN32
        dlerror(); /* clear error code */
        void* dllhandle = dlopen(lib, RTLD_LAZY);
        componentsList.components[i].libHandle = dllhandle;
#define GETPROCADDRESS dlsym
#define GetLastError dlerror
#define Sleep(msec) sleep((int)msec/1000)
#else
        SetLastError(0); /* clear error code */
        HINSTANCE dllhandle = LoadLibrary(LPCSTR(lib));
        componentsList.components[i].libHandle = dllhandle;
#define GETPROCADDRESS GetProcAddress
#endif

        if (dllhandle == NULL) {

#ifndef _WIN32
            if ((err = dlerror()) != NULL)
                throw Exception(true, Exception::ERR_OS, "Cannot load component library \"%s\". Error: %s\n", lib, err);
#else
            if (GetLastError() == 193)
                throw Exception(true, Exception::ERR_OS, "Cannot load component library \"%s\". Return code: %d\n    Most probably a 32bit - 64bit conflict.", lib, GetLastError());
            else
                throw Exception(true, Exception::ERR_OS, "Cannot load component library \"%s\". Return code: %d", lib, GetLastError());
#endif
        }

        // Collect BMI entry points
        componentsList.components[i].dllInitialize = (BMI_INITIALIZE)GETPROCADDRESS(dllhandle, BmiInitializeEntryPoint);
        if (componentsList.components[i].dllInitialize == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiInitializeEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllUpdate = (BMI_UPDATE)GETPROCADDRESS(dllhandle, BmiUpdateEntryPoint);
        if (componentsList.components[i].dllUpdate == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiUpdateEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllFinalize = (BMI_FINALIZE)GETPROCADDRESS(dllhandle, BmiFinalizeEntryPoint);
        if (componentsList.components[i].dllFinalize == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiFinalizeEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllGetStartTime = (BMI_GETSTARTTIME)GETPROCADDRESS(dllhandle, BmiGetStartTimeEntryPoint);
        if (componentsList.components[i].dllGetStartTime == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetStartTimeEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllGetEndTime = (BMI_GETENDTIME)GETPROCADDRESS(dllhandle, BmiGetEndTimeEntryPoint);
        if (componentsList.components[i].dllGetEndTime == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetEndTimeEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllGetTimeStep = (BMI_GETTIMESTEP)GETPROCADDRESS(dllhandle, BmiGetTimeStepEntryPoint);
        if (componentsList.components[i].dllGetTimeStep == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetStartTimeEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllGetCurrentTime = (BMI_GETCURRENTTIME)GETPROCADDRESS(dllhandle, BmiGetCurrentTimeEntryPoint);
        if (componentsList.components[i].dllGetCurrentTime == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetCurrentTimeEntryPoint, lib, GetLastError());
        }

        componentsList.components[i].dllGetAttribute = (BMI_GETATTRIBUTE)GETPROCADDRESS(dllhandle, BmiGetAttributeEntryPoint);
        if (componentsList.components[i].dllGetAttribute == NULL) {
            log->Write(ALL, my_rank, "No GetAttribute entry point in %s !", componentsList.components[i].library);
        }
        //      If GetAttribute is optional in a lib, no need to throw an exception
        //      if (componentsList.components[i].dllGetStartTime == NULL) {
        //          throw Exception (true,  Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetAttributeEntryPoint, lib, GetLastError());
        //        }

        if (componentsList.components[i].type == COMP_TYPE_DEFAULT_BMI ||
            componentsList.components[i].type == COMP_TYPE_FM ||
            componentsList.components[i].type == COMP_TYPE_RTC ||
            componentsList.components[i].type == COMP_TYPE_RR ||
            componentsList.components[i].type == COMP_TYPE_FLOW1D ||
            componentsList.components[i].type == COMP_TYPE_FLOW1D2D ||
            componentsList.components[i].type == COMP_TYPE_DELWAQ ||
            componentsList.components[i].type == COMP_TYPE_COSUMO_BMI ||
            componentsList.components[i].type == COMP_TYPE_ZSF  ||
            componentsList.components[i].type == COMP_TYPE_TEST ||
            componentsList.components[i].type == COMP_TYPE_WANDA) {
            // RTC-Tools: setVar is used
            componentsList.components[i].dllSetVar = (BMI_SETVAR)GETPROCADDRESS(dllhandle, BmiSetVarEntryPoint);
            if (componentsList.components[i].dllSetVar == NULL) {
                throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiSetVarEntryPoint, lib, GetLastError());
            }
        }
        else {
            componentsList.components[i].dllSetVar = NULL;
        }

        if (componentsList.components[i].type == COMP_TYPE_DEFAULT_BMI ||
            componentsList.components[i].type == COMP_TYPE_FM ||
            componentsList.components[i].type == COMP_TYPE_ZSF ||
            componentsList.components[i].type == COMP_TYPE_COSUMO_BMI) {
            componentsList.components[i].dllGetVarShape = (BMI_GETVARSHAPE)GETPROCADDRESS(dllhandle, BmiGetVarShapeEntryPoint);
            if (componentsList.components[i].dllGetVarShape == NULL) {
                throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetVarShapeEntryPoint, lib, GetLastError());
            }
        }
        else {
            componentsList.components[i].dllGetVarShape = NULL;
        }

        // BMILogger callback: FLOW1D uses BmiSetLogger
        if (componentsList.components[i].type == COMP_TYPE_FLOW1D) {
            componentsList.components[i].setLogger = (BMI_SET_LOGGER)GETPROCADDRESS(dllhandle, BmiSetLogger);
            if (componentsList.components[i].setLogger == NULL) {
                throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiSetLogger, lib, GetLastError());
            }
            componentsList.components[i].setLogger((BMILogger)&_log);
            double level = (double)this->logLevel;
            componentsList.components[i].dllSetVar("debugLevel", (const void*)&level);
        }
        // BMILogger callback: FLOWFM uses BmiSetLogger2
      if (componentsList.components[i].type == COMP_TYPE_FM || componentsList.components[i].type == COMP_TYPE_COSUMO_BMI) {
            componentsList.components[i].setLogger = (BMI_SET_LOGGER)GETPROCADDRESS(dllhandle, BmiSetLogger);
            if (componentsList.components[i].setLogger == NULL) {
                throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiSetLogger, lib, GetLastError());
            }
            componentsList.components[i].setLogger((BMILogger)&_log);
            // Is it possible to set the debugLevel in FM? componentsList.components[i].dllSetVar("debugLevel", (const void *)&level);
        }

        componentsList.components[i].dllGetVar = (BMI_GETVAR)GETPROCADDRESS(dllhandle, BmiGetVarEntryPoint);
        if (componentsList.components[i].dllGetVar == NULL) {
            throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, "Cannot find function \"%s\" in library \"%s\". Return code: %d", BmiGetVarEntryPoint, lib, GetLastError());
        }

        delete[] lib;
    }
    printComponentVersionStrings(INFO);            // List component version to log
}

//void Dimr::printComponentVersionStrings (Level my_level) {
void Dimr::printComponentVersionStrings(Level my_level) {
    const char* version = "version";
    char* versionstr = new char[MAXSTRINGLEN];
    int ierr;

    for (int i = 0; i < componentsList.numComponents; i++) {
        strcpy(versionstr, "");

        if (componentsList.components[i].processes[0] == my_rank) {
            if (componentsList.components[i].dllGetAttribute != NULL) {
                componentsList.components[i].dllGetAttribute(version, versionstr);
            }
        }
        if (componentsList.components[i].numProcesses < numranks) {
            ierr = MPI_Bcast(versionstr, MAXSTRINGLEN, MPI_CHAR, componentsList.components[i].processes[0], MPI_COMM_WORLD);
        }

        if (strlen(versionstr) == 0) {
            strcpy(versionstr, "Unknown");
        }
        if (my_rank == 0) {
            if (i == 0) {
                log->Write(my_level, my_rank, "");
                log->Write(my_level, my_rank, "Version Information of Components");
                log->Write(my_level, my_rank, "=================================");
            }
            log->Write(my_level, my_rank, "%-35s: %s", componentsList.components[i].library, versionstr);
        }
    }

    if (my_rank == 0) {
        log->Write(my_level, my_rank, "---------------------------------");
        log->Write(my_level, my_rank, "");
    }

    delete[] versionstr;
}

//------------------------------------------------------------------------------
void Dimr::freeLibs(void) {

    //          linux windows   mac
    // lib        so    dll     dylib
    // module     so    dll     so

#if defined (OSX)
    // Macintosh:VERY SIMILAR TO LINUX
    throw Exception(true, Exception::ERR_OS, "ABORT: %s has not be ported to Apple Mac OS/X yet", exeName);
#endif
#ifndef _WIN32
    char* err;
#endif

    // do for all libraries
    for (int i = 0; i < componentsList.numComponents; i++) {
        if (!componentsList.components[i].onThisRank) {
            continue;
        }

        log->Write(ALL, my_rank, "Freeing library \"%s\"", componentsList.components[i].library);
#ifndef _WIN32
        dlerror(); /* clear error code */
        int ierr = dlclose(componentsList.components[i].libHandle);
        if ((err = dlerror()) != NULL) {
            throw Exception(true, Exception::ERR_OS, "Cannot free component library \"%s\". Error: %s\n", componentsList.components[i].library, err);
        }
#else
        DWORD ierr;
        SetLastError(0); /* clear error code */
        bool success = FreeLibrary(componentsList.components[i].libHandle);
        if ((ierr = GetLastError()) != 0) {
            throw Exception(true, Exception::ERR_OS, "Cannot free component library \"%s\". Return code: %d.", componentsList.components[i].library, ierr);
        }
#endif

    }
}


//------------------------------------------------------------------------------
void Dimr::processWaitFile(void) {
    // The following waitFile code is introduced for
    // debugging parallel runs. It should NOT be used for any other purpose!

    XmlTree* rootXml = static_cast<XmlTree*>(config->Lookup("/dimrConfig"));
    const char* waitFile = rootXml->GetElement("waitFile");
    if (waitFile != NULL) {
        log->Write(INFO, my_rank, "Waiting for file \"%s\" to appear...", waitFile);
        fflush(stdout);
        FILE* f;
        do {
            f = fopen(waitFile, "r");
            Sleep(1000);
        } while (f == NULL);
        log->Write(INFO, my_rank, "Found waitfile \"%s\".", waitFile);
        fclose(f);
    }
    ready = 1;
}



//------------------------------------------------------------------------------
void Dimr::timersInit(void) {
    for (int i = 0; i < componentsList.numComponents; i++) {
        componentsList.components[i].timerSum = 0;
        componentsList.components[i].timerStart = 0;
    }
}



//------------------------------------------------------------------------------
void Dimr::timerStart(dimr_component* thisComponent) {
    thisComponent->timerStart = clock->Epoch();
}



//------------------------------------------------------------------------------
void Dimr::timerEnd(dimr_component* thisComponent) {
    Clock::Timestamp curtime = clock->Epoch();
    thisComponent->timerSum = curtime - thisComponent->timerStart + thisComponent->timerSum;
    thisComponent->timerStart = 0;
}



//------------------------------------------------------------------------------
void Dimr::timersFinish(void) {
    log->Write(INFO, my_rank, "TIMER INFO:\n");
    for (int i = 0; i < componentsList.numComponents; i++) {
        componentsList.components[i].timerStart = 0;
        log->Write(INFO, my_rank, "%s\t: %d.%d sec", componentsList.components[i].name,
            componentsList.components[i].timerSum / 1000000,
            componentsList.components[i].timerSum % 1000000);
        componentsList.components[i].timerSum = 0.0;
    }
    timerFinish();
}
//------------------------------------------------------------------------------
void Dimr::timerFinish(void)
{
    Clock::Timestamp curtime = clock->Epoch();
    this->timerSumStamp = curtime - this->timerStartStamp + this->timerSumStamp;
    this->timerStartStamp = 0;
    log->Write(INFO, my_rank, "%s\t: %d.%d sec", "DIMR_LIB",
        this->timerSumStamp / 1000000,
        this->timerSumStamp % 1000000);
    this->timerSumStamp = 0;
}




//------------------------------------------------------------------------------
void Dimr::char_to_ints(char* line, int** iarr, int* count) {
    std::stringstream stream(line);
    // TODO: support also:
    // The processes may be specified as a space separated list with series compressed using colons
    //       e.g. 16:31

    int np = 0;
    int p;
    while (1) { // NOTE: there's no checking on a valid first number yet.
        stream >> p;
        if (!stream) {
            break;
        }
        np++;
    }
    *count = np;
    *iarr = (int*)malloc(*count * sizeof(int));

    stream.clear();
    stream.seekg(0);
    stream.str(line);
    np = 0;
    while (1) { // NOTE: there's no checking on a valid first number yet.
        stream >> (*iarr)[np];
        if (!stream) {
            break;
        }
        np++;
    }
}
//------------------------------------------------------------------------------
void Dimr::_log(Level level, const char* msg) {
    Dimr* thisDimr = GetInstance();
    thisDimr->log->Write(level, thisDimr->my_rank, msg);
}
