/*
!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
 *  Storm-event Parallelization for SOBEK-RR
 *
 *  The main routine for SOBEK-RR calls PARALLELL(N) (in FORTRAN-90) after all
 *  common initialization to simulate N independent storm events.  This module
 *  implements PARALLELL and calls BUI(I) for each storm event I.  BUI is
 *  implemented elsewhere (again in F90).
 *
 *  Since the number of storms may (greatly) exceed the number of processors,
 *  this module limits the number of simultaneous processes to the value of the
 *  environment variable SOBEK_RR_MAX_PROCS.  If undefined, it defaults to 1.
 *  If zero, no limit is placed on the number of processes (i.e., a process for
 *  each storm will be created immediately).
 *
 *  WARNING: Sending (uncaught) signals to individual event processes will
 *  cause this module to treat such processes as terminated.  This will screw
 *  everything up (albeit with a warning);  ToDo: define signal and (abnormal)
 *  termination protocol.  In particular, the STOP and CONT signals are useful.
 *
 *******************************************************************************
 *  TEMPORARY HACK:
 *  After a storm event has been simulated, execute a SOBEK-FLOW for that
 *  event.  The name of the executable and its input file are specified by
 *  the environment variables SOBEK_RR_FLOW_EXE and SOBEK_RR_FLOW_CONFIG.
 *******************************************************************************
 *
 *  Compliance: this module is ANSI-C and POSIX compliant, but has only been
 *  tested on IRIX 6.5 to date.  The interface between FORTRAN-90 and C is
 *  not guaranteed to work on non-IRIX systems.
 *
 *
 *  Irv.Elshoff@deltares.nl
 *  22 mar 00
 *
 *  Copyright (C) 2000, Deltares
 ******************************************************************************/


/*
 * Only applicable for linux environment
 */
#if defined(USE_UNIX)

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>


#define	ENVAR	    "SOBEK_RR_MAX_PROCS"    /* name of environment variable */

#define	min(A,B)    (((A) < (B)) ? (A) : (B))
#define	max(A,B)    (((A) > (B)) ? (A) : (B))

#define	MAXSTR	    4096		    /* max size of a string (e.g., pathname) */


/*------------------------------------------------------------------------------
 *  Type definitions
 */

/*
 *  Event table.
 *  Used to keep track of process times and status.
 *  Informative.
 */

typedef struct event_st {
    pid_t   pid;	    /* process ID */
    time_t  startTime;	    /* when process was created */
    time_t  stopTime;	    /* when process terminated */
    int	    pstat;	    /* process exit status */
    } event;


/*------------------------------------------------------------------------------
 *  Global variable definitions
 */

char	flow_exe    [MAXSTR];
char	flow_input  [MAXSTR];



/*------------------------------------------------------------------------------
 *  Function declarations
 */


static void StartEvent (int, int, event *);
static void WaitForEventTermination (int, event *);
static void SummarizeEventTable (int, event *);


#if (defined(__cplusplus)||defined(_cplusplus))
extern "C" {
#endif

    void parallell_( int *pNumEvents );
    extern void bui_ (int *);

#if (defined(__cplusplus)||defined(_cplusplus))
}
#endif


/*------------------------------------------------------------------------------
 *  Public functions
 */

void
parallell_ (		    /*** FORTRAN-90 -> C INTERFACE ***/
    int *   pNumEvents
    ) {

    /*
     *	This is the main routine, called by SOBEK_RR
     */

    int	    nEvents = *pNumEvents;	/* number of storm events */
    int	    nProcs;			/* max num of parallel processes */
    int	    nWait = 0;			/* num processes to wait for */
    event * evTab;			/* event table */
    int	    i;
    char    *s;


    /*
     *	Create the event table
     */

    if ((evTab = (event *) malloc ((nEvents+1) * sizeof (struct event_st))) == NULL) {
	fprintf (stderr, "Cannot allocate event table for %d storms\n", nEvents);
	exit (1);
	}

    for (i = 1 ; i <= nEvents ; i++) {
	evTab[i].pid = (pid_t) 0;
	evTab[i].startTime = evTab[i].stopTime = (time_t) 0;
	evTab[i].pstat = 0;
	}


    /*
     *	Set the maximum number of processes
     */

    if ((s = getenv (ENVAR)) == NULL ||
	    sscanf (s, "%d", &nProcs) != 1 ||
	    nProcs < 0
	    ) {
	nProcs = 1;
	}

    if (nProcs == 0)  nProcs = nEvents;

    fprintf (stderr, "Simulating %d storms on %d processors...\n", nEvents, nProcs);


    /*
     *	TEMPORARY HACK:
     *	Get environment variables for post-event SOBEK FLOW execution
     */

    if ((s = getenv ("SOBEK_RR_FLOW_EXE")) == NULL) {
	flow_exe[0] = '\0';
	}
    else {
	if (strlen (s) >= MAXSTR) {
	    fprintf (stderr, "Value of envar SOBEK_RR_FLOW_EXE too long\n");
	    exit (1);
	    }
	strcpy (flow_exe, s);
	}

    if ((s = getenv ("SOBEK_RR_FLOW_CONFIG")) != NULL) {
	if (strlen (s) >= MAXSTR) {
	    fprintf (stderr, "Value of envar SOBEK_RR_FLOW_CONFIG too long\n");
	    exit (1);
	    }
	strcpy (flow_input, s);
	}






    /*
     *	Create new processes for each storm event until the maximum number
     *	of processes has been reached.  Thereafter, wait for a process to
     *	terminate before creating a new one.
     */

    for (i = 1 ; i <= nEvents ; i++) {
	if (nProcs > 0) {
	    StartEvent (i, nEvents, &evTab[i]);
	    nProcs--;
	    nWait++;
	    }
	else {
	    WaitForEventTermination (nEvents, evTab);
	    StartEvent (i, nEvents, &evTab[i]);
	    }
	}


    /*
     *	Wait remaining processes to finish.
     *	Then print summary.
     */

    while (nWait-- > 0) {
	WaitForEventTermination (nEvents, evTab);
	}

    SummarizeEventTable (nEvents, evTab);
    }



/*------------------------------------------------------------------------------
 *  Private functions
 */

static void
StartEvent (
    int	    i,
    int	    nEvents,
    event * ev
    ) {

    /*
     *	Fork a process to simulate a storm event.
     *	Store begin and end times of actual simulation in event table.
     */

    if ((ev->pid = fork ()) == (pid_t) -1) {
	fprintf (stderr, "Error: Cannot create process for storm %d\n\t", i);
	perror (NULL);
	exit (1);
	}

    else if (ev->pid == (pid_t) 0) {	/* child */
	ev->startTime = time (NULL);
	bui_ (&i);			/*** C -> FORTRAN-90 INTERFACE ***/
	ev->stopTime = time (NULL);
	/* exit (0); */


	/* TEMPORARY HACK */

	if (strlen (flow_exe) != 0) {
	    char si [MAXSTR], sn [MAXSTR];
	    sprintf (si, "%d", i);
	    sprintf (sn, "%d", nEvents);

	    execl (flow_exe, flow_exe, flow_input, si, sn, NULL);

	    fprintf (stderr, "Cannot execute %s for storm event %d\n", flow_exe, i);
	    exit (1);
	    }
	else {
	    exit (0);
	    }
	}
    }


static void
WaitForEventTermination (
    int nEvents,
    event *evTab
    ) {

    /*
     *	Wait for a process to terminate.
     *	Store exit status in event table.
     */

    pid_t   pid;			/* process ID */
    int	    pstat;			/* process exit status */
    int	    i;


    if ((pid = wait (&pstat)) < 0) {
	fprintf (stderr, "Error: wait fails; runaway processes may exist!\n");
	exit (1);
	}

    if (! WIFEXITED (pstat)) {
	fprintf (stderr, "Warning: wait detects uncaught child signal!\n");
	}

    for (i = 1 ; i <= nEvents ; i++) {
	if (pid == evTab[i].pid) {
	    evTab[i].pstat = pstat;
	    break;
	    }
	}

    if (i > nEvents) {
	fprintf (stderr, "Error: terminated process not in event table ?!\n");
	exit (1);
	}
    }


static void
SummarizeEventTable (
    int nEvents,
    event *evTab
    ) {

    /*
     *	Print a summary of the event table.
     */

    int	    i;
    int	    aggregateElapsed = 0;
    int	    totalElapsed;
    time_t  beginTime = evTab[1].startTime;
    time_t  endTime = evTab[1].stopTime;

    fprintf (stderr, "\n\n-----------------------------------------------\n");
    fprintf (stderr, "EVENT PROCESS SUMMARY\n");
    fprintf (stderr, "Event\tElapsed(sec)\tStatus\n");

    for (i = 1 ; i <= nEvents ; i++) {
	int diff = evTab[i].stopTime - evTab[i].startTime;

	aggregateElapsed += diff;
	beginTime = min (beginTime, evTab[i].startTime);
	endTime   = max (endTime,   evTab[i].stopTime);

	fprintf (stderr, "%4d\t%7d\t\t  0x%x\n",
				i,
				diff,
				evTab[i].pstat
				);
	}



    totalElapsed = endTime - beginTime;
    fprintf (stderr, "\nAggregate elapsed time = %d sec\n",
			    aggregateElapsed);
    fprintf (stderr, "Real elapsed time = %d sec\n",
			    totalElapsed);

    if (totalElapsed > 0) {
	fprintf (stderr, "Ratio = %5.2f sec\n",
				((float) aggregateElapsed) / (totalElapsed));
	}

    fprintf (stderr, "-----------------------------------------------\n\n");
    fflush (stderr);
    }


#endif