      subroutine wqfile ( laux   ,istep  ,nots   ,
     +                    time   ,npntr  ,nposeg ,tofrom ,
     +                    segfun ,sexar  ,sexfl  ,svol   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFILE (Water Quality output FILE generation)
c
c Module description: Write information to binary output files
c
c                     This routine will create the binary output files
c                     for step one. After that for each time step this
c                     routine will be called to append the information
c                     in the files. If the last time step has been pro-
c                     cessed the output file will be closed. For appen-
c                     ding the information routine WQFWRT will be cal-
c                     led.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 istep             I  Current time step number (t(n+1)).
c  2 laux              I  TRUE=Auxiliary files must be created.
c  4 nots              I  Maximum number of time steps.
c  6 npntr             I  Number of entries in pntr table.
c  7 nposeg            I  Number of positive segment numbers.
c  9 segfun            P  -
c 10 sexar             P  -
c 11 sexfl             P  -
c 12 svol              P  -
c  5 time              P  -
c  8 tofrom            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqficr  Water Quality FIle CReate
c wqfwrt  Water Quality File WRiTe
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
c $Log: wqfile.pf,v $
c Revision 1.6  1999/03/15  15:53:56  kuipe_j
c tabs removed
c
c Revision 1.5  1998/02/13  12:13:03  kuipe_j
c Adapt to CMT
c
c Revision 1.4  1997/01/20  13:14:20  kuipe_j
c Remove auz. output
c
c Revision 1.3  1996/12/20  15:45:31  kuipe_j
c Extra record at end
c
c Revision 1.2  1995/05/30  07:08:29  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:51  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:37  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\filsim.i'
c
c     Parameters
c
      logical        laux
c
      integer        istep  ,nots   ,npntr  ,nposeg, time
c
      real           tofrom (2,npntr),
     +               segfun (nposeg,4),
     +               sexar  (npntr),
     +               sexfl  (npntr),
     +               svol   (nposeg)
c
c     Variables
c
      integer        tdummy, nowrt
c
      integer        lusgf,
     +               luexa,
     +               luexf,
     +               lulen,
     +               luvol
c
      parameter     (lusgf = 20,
     +               luexa = 21,
     +               luexf = 22,
     +               lulen = 23,
     +               luvol = 24
     +              )
c
c     Open and create files if step = 1
c
      if (istep .eq. 1) then
         if (laux) then
c
c           Segment functions file: contains
c
c           1. Chezy
c           2. Horizontal surfaces
c           3. Velocities
c           4. Widths
c
            call wqficr (lusgf, fsgfun )
         endif
c
c        Exchange areas
c
         call wqficr (luexa, fexare )
c
c        Exchange flows
c
         call wqficr (luexf, fexflo )
c
c        From and to lenghts
c
         call wqficr (lulen, flenth )
c
c        Volumes
c
         call wqficr (luvol, fvolum )
      endif
c
c     Write from and to lengths only first time step
c
      if (istep .eq. 1) then
         tdummy = 0
         nowrt  = 2 * npntr
         call wqfwrt (lulen, tdummy, nowrt, tofrom )
      endif
c
c     Append exchange area and exchange flows
c
      call wqfwrt (luexa, time, npntr , sexar)
      call wqfwrt (luexf, time, npntr , sexfl)
c
c     Append volumes
c
      call wqfwrt (luvol, time, nposeg, svol )
c
c     Append chezy, horizontal surfaces, velocities and widths
c
      if (laux) then
         nowrt = 4 * nposeg
         call wqfwrt (lusgf , time, nowrt , segfun )
      endif
c
c     Close files if last time step has been processed
c
      if (istep .eq. nots) then
         if (laux) then
            close ( unit = lusgf )
         endif
c
c        Write dummy record at a time > simulation time
c        to prevent rewinding in Delwaq
c
         call wqfwrt (luvol, time+100, nposeg, svol )
c
         close ( unit = luexa )
         close ( unit = luexf )
         close ( unit = luvol )
         close ( unit = lulen )
      endif
c
      return
      end
