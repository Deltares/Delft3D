subroutine wqfile ( laux   ,istep  ,nots   ,&
&time   ,npntr  ,nposeg ,tofrom ,&
&segfun ,sexar  ,sexfl  ,svol   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFILE (Water Quality output FILE generation)
!
! Module description: Write information to binary output files
!
!                     This routine will create the binary output files
!                     for step one. After that for each time step this
!                     routine will be called to append the information
!                     in the files. If the last time step has been pro-
!                     cessed the output file will be closed. For appen-
!                     ding the information routine WQFWRT will be cal-
!                     led.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 istep             I  Current time step number (t(n+1)).
!  2 laux              I  TRUE=Auxiliary files must be created.
!  4 nots              I  Maximum number of time steps.
!  6 npntr             I  Number of entries in pntr table.
!  7 nposeg            I  Number of positive segment numbers.
!  9 segfun            P  -
! 10 sexar             P  -
! 11 sexfl             P  -
! 12 svol              P  -
!  5 time              P  -
!  8 tofrom            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqficr  Water Quality FIle CReate
! wqfwrt  Water Quality File WRiTe
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
! $Log: wqfile.pf,v $
! Revision 1.6  1999/03/15  15:53:56  kuipe_j
! tabs removed
!
! Revision 1.5  1998/02/13  12:13:03  kuipe_j
! Adapt to CMT
!
! Revision 1.4  1997/01/20  13:14:20  kuipe_j
! Remove auz. output
!
! Revision 1.3  1996/12/20  15:45:31  kuipe_j
! Extra record at end
!
! Revision 1.2  1995/05/30  07:08:29  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:51  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:37  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
!     Parameters
!
   logical        laux
!
   integer        istep  ,nots   ,npntr  ,nposeg, time
!
   real           tofrom (2,npntr),&
   &segfun (nposeg,4),&
   &sexar  (npntr),&
   &sexfl  (npntr),&
   &svol   (nposeg)
!
!     Variables
!
   integer        tdummy, nowrt
!
   integer        lusgf,&
   &luexa,&
   &luexf,&
   &lulen,&
   &luvol
!
   parameter     (lusgf = 20,&
   &luexa = 21,&
   &luexf = 22,&
   &lulen = 23,&
   &luvol = 24&
   &)
!
!     Open and create files if step = 1
!
   if (istep .eq. 1) then
      if (laux) then
!
!           Segment functions file: contains
!
!           1. Chezy
!           2. Horizontal surfaces
!           3. Velocities
!           4. Widths
!
         call wqficr (lusgf, fsgfun )
      endif
!
!        Exchange areas
!
      call wqficr (luexa, fexare )
!
!        Exchange flows
!
      call wqficr (luexf, fexflo )
!
!        From and to lenghts
!
      call wqficr (lulen, flenth )
!
!        Volumes
!
      call wqficr (luvol, fvolum )
   endif
!
!     Write from and to lengths only first time step
!
   if (istep .eq. 1) then
      tdummy = 0
      nowrt  = 2 * npntr
      call wqfwrt (lulen, tdummy, nowrt, tofrom )
   endif
!
!     Append exchange area and exchange flows
!
   call wqfwrt (luexa, time, npntr , sexar)
   call wqfwrt (luexf, time, npntr , sexfl)
!
!     Append volumes
!
   call wqfwrt (luvol, time, nposeg, svol )
!
!     Append chezy, horizontal surfaces, velocities and widths
!
   if (laux) then
      nowrt = 4 * nposeg
      call wqfwrt (lusgf , time, nowrt , segfun )
   endif
!
!     Close files if last time step has been processed
!
   if (istep .eq. nots) then
      if (laux) then
         close ( unit = lusgf )
      endif
!
!        Write dummy record at a time > simulation time
!        to prevent rewinding in Delwaq
!
      call wqfwrt (luvol, time+100, nposeg, svol )
!
      close ( unit = luexa )
      close ( unit = luexf )
      close ( unit = luvol )
      close ( unit = lulen )
   endif
!
   return
end
