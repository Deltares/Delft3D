subroutine IVBDSM(time   ,dt     ,istep  ,nstep  ,first ,wrirst ,&
&ngrid  ,nbran  ,branch ,x      ,qp    ,hp     ,&
&hmax   ,gridnm )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             IVBDSM (special feature for IVBDoS, Main routine)
!
! Module description:
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 time              I  Actual time level (at t=n+1) in sec.
!  2 dt                I  Previous time level (at t=n) in sec.
!  3 istep             I  Current time step number (t(n+1)).
!  4 nstep             I  Last time step number.
!  5 first             I  True in case of first call.
!  6 wrirst            I  True when restart info must be written.
!  7 ngrid             I  Number of grid points in network.
!  8 nbran             I  Number of branches.
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 10 x(ngrid)          I  Coordinates in every grid point.
! 11 qp(ngrid)         I  Discharge in every grid point
! 12 h(ngrid)          I  Water level in every grid point at t=n+1
!                         (TIME).
! 13 gridnm(ngrid)     I  Name of every grid point.
! 14 hmax(ngrid)      IO  Maximum water level in every grid point.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! IVBOUT  IVBdos OUTput of maximum water levels
!
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: ivbdsm.pf,v $
! Revision 1.1  1999/03/15  14:30:14  kuipe_j
! IVBDOS
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer          istep ,nstep    ,ngrid      ,nbran
   integer          branch(4,nbran)
   real             x     (ngrid)   ,hmax(ngrid)
   double precision time  ,dt, qp(ngrid,3),hp(ngrid,3)
   logical          first ,wrirst
   character*40     gridnm(ngrid)

   logical          ivbact,maxwl,restrt
   common  /ivbdos/ ivbact,maxwl,restrt
!
!     Declaration of local variables:
!
   integer          kode
   double precision timep

!    Set flags false. These flags will be overruled if
!    there is a IVBDOS.INI file

   if (first) then
      maxwl  = .false.
      restrt = .false.
      ivbact = .false.
   endif

   timep = time - dt
   call IVBCLS(time   ,timep  ,ngrid  ,nbran  ,branch  ,&
   &x      ,gridnm ,qp(1,3),hp(1,3),qp(1,1) )

!     In case of IVBDOS.INI restart will only be written
!     if flag is set

   if (ivbact) wrirst = restrt

   if (maxwl) then
      if (first) then
         kode = 1
      else if (istep.eq.nstep) then
         kode = 3
      else
         kode = 2
      endif
      call IVBOUT(kode  ,ngrid  ,x     ,hp(1,3),hmax   ,&
      &nbran ,branch )
   endif

end
