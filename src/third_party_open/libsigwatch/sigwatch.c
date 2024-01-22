/*
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    libsigwatch copyright 2003, Norman Gray
 *    http://www.astro.gla.ac.uk/users/norman/
 *    norman@astro.gla.ac.uk
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

static int lastsignal;
#if defined(HAVE_NSIG)
#define NSIGS NSIG
#elif defined(HAVE__NSIG)
#define NSIGS _NSIG
#else
#define NSIGS 32
#endif

#ifndef F77_FUNC
/* configure.ac AC_F77_WRAPPERS was supposed to configure this, but
 * clearly didn't.  Use a common default.
 */
#define F77_FUNC(lcname,ucname) lcname ## _
#endif

#define WATCHSIGNAL_F     F77_FUNC(watchsignal,WATCHSIGNAL)
#define WATCHSIGNALNAME_F F77_FUNC(watchsignalname,WATCHSIGNALNAME)
#define GETLASTSIGNAL_F   F77_FUNC(getlastsignal,GETLASTSIGNAL)
#define SIGWATCHVERSION_F F77_FUNC(sigwatchversion,SIGWATCHVERSION)

static int signalresponses[NSIGS] = { -1 }; /* not-initialised flag */


int WATCHSIGNAL_F    (int *signum);
int WATCHSIGNALNAME_F(char *signame, int *response, int len);
int GETLASTSIGNAL_F  (void);

static void detectsignal(int signum);
static void initresponses(void);

typedef void (*sighandler_t)(int);

/*
 * Watch for the given signal.
 */
int WATCHSIGNAL_F(int *signump)
{
    sighandler_t previous;
    int signum = *signump;
    int rval;
    
    initresponses();
    previous = signal(signum, (sighandler_t)detectsignal);
    if (previous == SIG_ERR) {
        rval = -1;
    } else {
        if (signum < NSIGS)
            signalresponses[signum] = signum;
        if (previous == SIG_DFL)
            rval = 0;
        else 
            /* There was a signal handler already -- warn about this */
            rval = 1;
    }
    return rval;
}
        

/*
 * Watch for the signal which has the given name, and when it is
 * found, have getlastsignal() return the given response.  If
 * response is 0 or *response is negative, then simply return the
 * signal number.
 *
 * The set of `named' signals is HUP, INT, USR1 and USR2.  For other
 * signals, use the numeric function, watchsignal().
 */
int WATCHSIGNALNAME_F(char *signame, int *response, int signame_length)
{
    int signum;
    sighandler_t previous;
    int rval;

#define SIGNAMEIS(str,n) signame_length >= (n) && \
    strncmp(signame, str, (n)) == 0

    initresponses();

    if (0)
        ; // dummy first case to allow always-correct 'else if' macro-expansion below
#ifdef SIGHUP
    else if (SIGNAMEIS("HUP", 3))
        signum = SIGHUP;
#endif
    else if (SIGNAMEIS("INT", 3))
        signum = SIGINT;
#ifdef SIGUSR1
    else if (SIGNAMEIS("USR1", 4))
        signum = SIGUSR1;
#endif
#ifdef SIGUSR2
    else if (SIGNAMEIS("USR2", 4))
        signum = SIGUSR2;
#endif
    else 
        signum = -1;

    if (signum >= 0) {
        /* All OK */
        previous = signal(signum, (sighandler_t)detectsignal);
        if (previous == SIG_ERR)
            rval = -1;
        else {
            if (signum < NSIGS) {
                if (response != 0 && *response > 0)
                    signalresponses[signum] = *response;
                else
                    signalresponses[signum] = signum;
            }
            if (previous == SIG_DFL)
                rval = 0;
            else
                /* There was a signal handler already -- warn about this */
                rval = 1;
        }
    } else {
        /* Unrecognised signal name */
        rval = -1;
    }
    return rval;
}

/* Return the response associated with the last signal, as registered
 * with watchsignal() or watchsignalname() 
 */
int GETLASTSIGNAL_F(void)
{
    int lastsig = lastsignal;
    lastsignal = 0;
    
    if (lastsig < NSIGS)
        return signalresponses[lastsig];
    else
        return lastsig;
}

int SIGWATCHVERSION_F(void)
{
    char *verstring = PACKAGE_VERSION;	/* format "major.minor" */
    char *endp;
    int rval;

    rval = strtoul(verstring, &endp, 10);
    rval *= 1000;
    if (*endp != '\0') {
	endp++;			/* increment past '.' */
	rval += strtoul(endp, 0, 10);
    }
    return rval;
}

/* This is the signal handler.  Simply store the signal. */
static void detectsignal(int signum)
{
    lastsignal = signum;
}

/* Initialise the signalresponses array */
static void initresponses(void)
{
    if (signalresponses[0] < 0) { /* not-initialised flag */
        int i;
        for (i=0; i<NSIGS; i++)
            signalresponses[i] = 0;
    }
}

