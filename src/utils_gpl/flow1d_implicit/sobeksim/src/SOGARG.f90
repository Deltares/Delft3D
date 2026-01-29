subroutine sogarg ( filnam, ker, lbatch , ldebug )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOGARG (SObek Get ARGument)
!
! Module description: Read passed runtime argument.
!
!                     If sobek is started one argument will be passed.
!                     This argument can be a model file name or a batch
!                     file name. The syntax is:
!
!                            sobek model file name     or
!                            sobek #batch file name
!
!                     The model file name should not contain an exten-
!                     sion. Extensions are appended by the sobek pro-
!                     gram. The batch file contains one or model model
!                     file names without extension. To fetch the argu-
!                     ment a system call is made. This call can differ
!                     for the different platforms (unix or dos).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 filnam            IO Filename of model to be processed without
!                         extension
!  2 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 lbatch            O  Switch to indicate batch mode
!  4 ldebug            O  Switch to enable debug mode
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getarg
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sogarg.pf,v $
! Revision 1.10  1999/03/15  15:19:46  kuipe_j
! tabs removed
!
! Revision 1.9  1997/05/28  12:52:56  kuipe_j
! initialize
!
! Revision 1.8  1996/01/17  14:47:36  kuipe_j
! header update
!
! Revision 1.7  1995/11/24  15:15:33  hoeks_a
! Aux. output removed
!
! Revision 1.6  1995/10/17  10:12:37  hoeks_a
! Some small changes especially to makefiles, found by porting to
! a DEC-ALPHA station.
!
! Revision 1.5  1995/09/22  10:04:10  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:11:31  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.3  1995/05/30  09:56:53  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:47  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:12  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:48  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   character*(*) filnam
   integer       ker
   logical       lbatch, ldebug
!
!     Variables
!
   integer       i, lname, narg
   integer*2     status
   character*120 buffer
!
!     Function to retrieve number of arguments
!
#if defined(USE_MSWINDOWS)
!
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
!
!
   filnam = buffer
!
!        Check for batch file
!
   if (filnam(1:1) .eq. '#') then
      lbatch = .true.
!
!        Remove # character from filename
!
      lname  = len (filnam)
      do 100 i = 1, lname - 1
         filnam(i:i) = filnam(i+1:i+1)
100   continue
      filnam(lname:lname) = ' '
   else
      lbatch = .false.
   endif

   if (filnam(1:1).eq.' ') then
      ker = 3
      stop'usage: sobeksim.exe <SOBEKSIM.INI-file>'
   endif

!
!        test for debug switch
!
   if (narg > 2) then
      ldebug = .true.
   else
      ldebug = .false.
   endif


   return
end
