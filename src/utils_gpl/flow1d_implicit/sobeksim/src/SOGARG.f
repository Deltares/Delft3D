      subroutine sogarg ( filnam, ker, lbatch , ldebug )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOGARG (SObek Get ARGument)
c
c Module description: Read passed runtime argument.
c
c                     If sobek is started one argument will be passed.
c                     This argument can be a model file name or a batch
c                     file name. The syntax is:
c
c                            sobek model file name     or
c                            sobek #batch file name
c
c                     The model file name should not contain an exten-
c                     sion. Extensions are appended by the sobek pro-
c                     gram. The batch file contains one or model model
c                     file names without extension. To fetch the argu-
c                     ment a system call is made. This call can differ
c                     for the different platforms (unix or dos).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 filnam            IO Filename of model to be processed without
c                         extension
c  2 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  3 lbatch            O  Switch to indicate batch mode
c  4 ldebug            O  Switch to enable debug mode
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getarg
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sogarg.pf,v $
c Revision 1.10  1999/03/15  15:19:46  kuipe_j
c tabs removed
c
c Revision 1.9  1997/05/28  12:52:56  kuipe_j
c initialize
c
c Revision 1.8  1996/01/17  14:47:36  kuipe_j
c header update
c
c Revision 1.7  1995/11/24  15:15:33  hoeks_a
c Aux. output removed
c
c Revision 1.6  1995/10/17  10:12:37  hoeks_a
c Some small changes especially to makefiles, found by porting to
c a DEC-ALPHA station.
c
c Revision 1.5  1995/09/22  10:04:10  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:11:31  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.3  1995/05/30  09:56:53  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:47  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:12  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:48  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      character*(*) filnam
      integer       ker
      logical       lbatch, ldebug
c
c     Variables
c
      integer       i, lname, narg
      integer*2     status
      character*120 buffer
c
c     Function to retrieve number of arguments
c
#if defined(USE_MSWINDOWS)
c
      narg = nargs()
      if (narg >= 2) then
         call getarg(1, buffer)
      else
         stop 'usage: sobeksim.exe <SOBEKSIM.INI-file>'
      endif
#endif
      
#if defined (USE_HPUX)
       call igetarg ( 1, buffer, 120 )
#endif
c
c
      filnam = buffer
c
c        Check for batch file
c
      if (filnam(1:1) .eq. '#') then
         lbatch = .true.
c
c        Remove # character from filename
c
         lname  = len (filnam)
         do 100 i = 1, lname - 1
            filnam(i:i) = filnam(i+1:i+1)
 100     continue
         filnam(lname:lname) = ' '
      else
         lbatch = .false.
      endif
      
      if (filnam(1:1).eq.' ') then
         ker = 3
         stop'usage: sobeksim.exe <SOBEKSIM.INI-file>'
      endif

c
c        test for debug switch
c
      if (narg > 2) then
         ldebug = .true.
      else
         ldebug = .false.
      endif


      return
      end
