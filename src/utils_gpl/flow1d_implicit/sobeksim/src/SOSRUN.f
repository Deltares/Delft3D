      subroutine SOSRUN (appl   ,lauto  ,lflow  ,lkalm  ,lsalt ,
     +                   lsedt  ,lmorp  ,lgrad  ,lwqin  ,ldlwq ,laux  ,
c                        mozart parameter plus logical grondwater
     +                   lmoza  , lgrwt ,
     +                   lrivr  ,lestu  ,itim   ,nstep  ,dtf   ,steady,
     +                   lwstat ,itp    ,ifp    ,ipc    ,ipcs  ,wqagst,
     +                   restrt ,newres ,idtm   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOSRUN (SObek Simulation RUN parameters)
c
c Module description: Extract run parameters from model input.
c
c                     This routine will read the time information which
c                     is needed for a simulation run.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 appl              I  Application string containing switches for
c                         simulation run
c 15 dtf               O  Time step flow module
c 24 idtm              O  Time step morphology module in whole numbers
c                         of flow step
c 19 ifp               O  Flow period in whole numbers of flow step
c 20 ipc               O  Play in period in whole numbers of flow period
c 13 itim(2)           O  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 18 itp               O  Tidal period in whole numbers of flow step
c  2 lauto             O  Switch to execute autostart procedure
c 10 laux              O  Switch to enable output of auxiliary water
c                         quality interface files
c  9 ldlwq             O  Switch to convert water quality files to del-
c                         waq input files
c 12 lestu             O  Switch to indicate estuary case
c  3 lflow             IO Switch to enable flow module
c  4 lkalm             O  -
c  7 lmorp             O  Logical indicator for morphology computation
c                         = .true.  : with morphology computation
c                         = .false. : without morphology computation
c 11 lrivr             O  Switch to indicate river case
c  5 lsalt             O  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  6 lsedt             O  Switch to enable sediment transport module
c  8 lwqin             O  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c 17 lwstat            O  True if status file must be written
c 23 newres            O  true, if a new restart file will be made
c 14 nstep             O  Last time step number in simulation.
c 22 restrt            O  Period of writing restart file (0 = write at
c                         end of run)
c 16 steady            O  Switch to enable a steady flow calculation
c 21 wqagst            O  Time step of the water quality process run.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c gtrpnt  GeT Real PoiNTer
c soipar  SObek Integer PARameter
c sorpar  SObek Real PARameter
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
c $Log: sosrun.pf,v $
c Revision 1.11  1999/03/15  15:19:53  kuipe_j
c tabs removed
c
c Revision 1.10  1996/09/03  14:33:46  kuipe_j
c frequency time hist, run in time est. morp
c
c Revision 1.9  1996/04/12  13:06:13  kuipe_j
c headers, minor changes
c
c Revision 1.8  1996/04/11  08:16:41  kuipe_j
c Kalman module added
c
c Revision 1.7  1996/01/17  14:47:42  kuipe_j
c header update
c
c Revision 1.6  1996/01/16  15:01:57  kuipe_j
c Restart improvements
c
c Revision 1.5  1995/10/18  09:01:09  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/22  10:04:35  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:08  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:02  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:28  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  08:28:47  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      character*(*) appl

      logical       lauto, lflow, lkalm, lsalt, lsedt,
     +              lmorp, lgrad, lwqin, ldlwq, laux,
     +              lrivr, lestu, newres,
c                   mozart declaration 
     +              lmoza, lgrwt


      integer       itim(2),nstep
      double precision      dtf
      logical       steady,lwstat
      integer       itp,   ifp,   ipc,   ipcs,  idtm
      integer       wqagst,restrt

c
c     Variables
c
      integer       ixrun, yyyy, mmdd, hhmm, sshh
c
c     External functions
c
      integer       gtrpnt, soipar
      real          sorpar
      external      gtrpnt, soipar, sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
c
c     Read and set logicals
c
      lauto = index ( appl, 'AUTO' ) .gt. 0
      lflow = index ( appl, 'FLOW' ) .gt. 0
      lkalm = index ( appl, 'KALM' ) .gt. 0
      lsalt = index ( appl, 'SALT' ) .gt. 0
      lsedt = index ( appl, 'SEDT' ) .gt. 0
      lmorp = index ( appl, 'MORP' ) .gt. 0
      lgrad = index ( appl, 'GRAD' ) .gt. 0
      lwqin = index ( appl, 'WQIN' ) .gt. 0
      ldlwq = index ( appl, 'DLWQ' ) .gt. 0
      laux  = index ( appl, 'AUXF' ) .gt. 0
      lrivr = index ( appl, 'RIVR' ) .gt. 0
      lestu = index ( appl, 'ESTU' ) .gt. 0
c     mozart flag
      lmoza = index ( appl, 'MOZA' ) .gt. 0
c     groundwater flag
      lgrwt = index ( appl, 'GRWT' ) .gt. 0
c
      if (lflow) then
c
c        Extract parameters
c
         ixrun   = gtrpnt ( 'FLWRUN')
c
c        Read begin date and time, end time, dt
c
         yyyy    = soipar ( rp(ixrun), 1 )
         mmdd    = soipar ( rp(ixrun), 2 )
         hhmm    = soipar ( rp(ixrun), 3 )
         sshh    = soipar ( rp(ixrun), 4 )
         nstep   = soipar ( rp(ixrun), 5 )
         dtf     = dble   (sorpar ( rp(ixrun), 6 ))
         wqagst  = soipar ( rp(ixrun), 7 )
         restrt  = soipar ( rp(ixrun), 8 )
         steady  = soipar ( rp(ixrun), 9 ) .eq. 0
         itp     = soipar ( rp(ixrun), 10)
         ifp     = soipar ( rp(ixrun), 11)
         ipc     = soipar ( rp(ixrun), 12)
         idtm    = soipar ( rp(ixrun), 13)
         lwstat  = soipar ( rp(ixrun), 14) .eq. 1
         newres  = soipar ( rp(ixrun), 15) .eq. 1
         ipcs    = soipar ( rp(ixrun), 16)
c
         itim(1) = yyyy * 10000 + mmdd
         itim(2) = hhmm * 10000 + sshh
      endif
c
      return
      end
