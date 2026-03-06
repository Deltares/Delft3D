subroutine SOSRUN (appl   ,lauto  ,lflow  ,lkalm  ,lsalt ,&
&lsedt  ,lmorp  ,lgrad  ,lwqin  ,ldlwq ,laux  ,&
!                        mozart parameter plus logical grondwater
&lmoza  , lgrwt ,&
&lrivr  ,lestu  ,itim   ,nstep  ,dtf   ,steady,&
&lwstat ,itp    ,ifp    ,ipc    ,ipcs  ,wqagst,&
&restrt ,newres ,idtm   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOSRUN (SObek Simulation RUN parameters)
!
! Module description: Extract run parameters from model input.
!
!                     This routine will read the time information which
!                     is needed for a simulation run.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 appl              I  Application string containing switches for
!                         simulation run
! 15 dtf               O  Time step flow module
! 24 idtm              O  Time step morphology module in whole numbers
!                         of flow step
! 19 ifp               O  Flow period in whole numbers of flow step
! 20 ipc               O  Play in period in whole numbers of flow period
! 13 itim(2)           O  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 18 itp               O  Tidal period in whole numbers of flow step
!  2 lauto             O  Switch to execute autostart procedure
! 10 laux              O  Switch to enable output of auxiliary water
!                         quality interface files
!  9 ldlwq             O  Switch to convert water quality files to del-
!                         waq input files
! 12 lestu             O  Switch to indicate estuary case
!  3 lflow             IO Switch to enable flow module
!  4 lkalm             O  -
!  7 lmorp             O  Logical indicator for morphology computation
!                         = .true.  : with morphology computation
!                         = .false. : without morphology computation
! 11 lrivr             O  Switch to indicate river case
!  5 lsalt             O  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  6 lsedt             O  Switch to enable sediment transport module
!  8 lwqin             O  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
! 17 lwstat            O  True if status file must be written
! 23 newres            O  true, if a new restart file will be made
! 14 nstep             O  Last time step number in simulation.
! 22 restrt            O  Period of writing restart file (0 = write at
!                         end of run)
! 16 steady            O  Switch to enable a steady flow calculation
! 21 wqagst            O  Time step of the water quality process run.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! gtrpnt  GeT Real PoiNTer
! soipar  SObek Integer PARameter
! sorpar  SObek Real PARameter
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
! $Log: sosrun.pf,v $
! Revision 1.11  1999/03/15  15:19:53  kuipe_j
! tabs removed
!
! Revision 1.10  1996/09/03  14:33:46  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.9  1996/04/12  13:06:13  kuipe_j
! headers, minor changes
!
! Revision 1.8  1996/04/11  08:16:41  kuipe_j
! Kalman module added
!
! Revision 1.7  1996/01/17  14:47:42  kuipe_j
! header update
!
! Revision 1.6  1996/01/16  15:01:57  kuipe_j
! Restart improvements
!
! Revision 1.5  1995/10/18  09:01:09  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/22  10:04:35  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:08  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:10:02  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:28  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  08:28:47  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   character*(*) appl

   logical       lauto, lflow, lkalm, lsalt, lsedt,&
   &lmorp, lgrad, lwqin, ldlwq, laux,&
   &lrivr, lestu, newres,&
!                   mozart declaration
   &lmoza, lgrwt


   integer       itim(2),nstep
   double precision      dtf
   logical       steady,lwstat
   integer       itp,   ifp,   ipc,   ipcs,  idtm
   integer       wqagst,restrt

!
!     Variables
!
   integer       ixrun, yyyy, mmdd, hhmm, sshh
!
!     External functions
!
   integer       gtrpnt, soipar
   real          sorpar
   external      gtrpnt, soipar, sorpar
!
!     Include memory pool
!
   include '..\include\mempool.i'
!
!     Read and set logicals
!
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
!     mozart flag
   lmoza = index ( appl, 'MOZA' ) .gt. 0
!     groundwater flag
   lgrwt = index ( appl, 'GRWT' ) .gt. 0
!
   if (lflow) then
!
!        Extract parameters
!
      ixrun   = gtrpnt ( 'FLWRUN')
!
!        Read begin date and time, end time, dt
!
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
!
      itim(1) = yyyy * 10000 + mmdd
      itim(2) = hhmm * 10000 + sshh
   endif
!
   return
end
