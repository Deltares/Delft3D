subroutine FLAREX (iter  ,option,hact  ,htop  ,hbase ,daext ,&
&overlp,delA  ,iart  ,delW)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAREX (FLow correction for ARea EXtra)
!
! Module description: The calculated flow/total area must be corrected
!                     with the extra area.
!
!                     Depending on the option the area is calculated in
!                     the concerning grid point. At the beginning of
!                     every time step (iter=1) "artop" is determined.
!                     For every iteration step the area is calculated.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 daext             I  Extra area (flow or total).
!  8 dela              O  Computed extra area depending on option.
! 10 delw              IO Computed extra width depending on option.
!  3 hact              I  Actual water level at gridpoint in branch.
!  5 hbase             I  base-level
!  4 htop              I  top-level
!  9 iart              IO Status-variable indicating rising or falling
!                         water level.
!  1 iter              I  Iteration step.
!  2 option            I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
!  7 overlp            I  adaptation height.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flarex.pf,v $
! Revision 1.5  1999/03/15  15:49:25  kuipe_j
! tabs removed
!
! Revision 1.4  1997/05/26  07:38:51  kuipe_j
! linearizatiom summerdikes introduced
!
! Revision 1.3  1996/04/12  13:03:37  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  iter  , option,iart
   real     hact  , htop  ,hbase ,daext ,overlp,delA, delW
!
!     Declaration of local variable:
!
   logical          artop
   real             htopp, epsdel, kwadr
!
   htopp = htop + overlp
   artop = iart .eq. 1
   delW  = 0.0
   kwadr = 0.5
   epsdel= kwadr * overlp
!
   if (option .eq. 1) then
      if (hact .gt. htopp) then
         artop = .false.
         delA  = daext
      else
         artop = .true.
         if (hact .gt. htop) then
            delW = daext / overlp / ( 1.0 - kwadr )
            if ( hact .lt. htop+epsdel) then
               delA = 0.5*(hact-htop)*(hact-htop)/epsdel*delW
               delW = (hact - htop)/epsdel*delW

            else if ( hact .gt. htopp-epsdel) then
               delA = daext -&
               &0.5*(htopp-hact)*(htopp-hact)/epsdel*delW
               delW = (htopp-hact)/epsdel*delW

            else
               delA = (hact-htop-0.5*epsdel)*delW
            endif
         else
            delA  = 0.
         endif
      endif
!
   else if (option .eq. 2) then
      if (iter .eq. 1) then
         if (hact .gt. htopp) artop = .false.
         if (hact .lt. hbase) artop = .true.
      endif
      if (artop) then
         if (hact .gt. htopp) then
            delA = daext
         else if (hact .gt. htop) then
            delW = daext / overlp / ( 1.0 - kwadr )
            if ( hact .lt. htop+epsdel) then
               delA = 0.5*(hact-htop)*(hact-htop)/epsdel*delW
               delW = (hact - htop)/epsdel*delW

            else if ( hact .gt. htopp-epsdel) then
               delA = daext -&
               &0.5*(htopp-hact)*(htopp-hact)/epsdel*delW
               delW = (htopp-hact)/epsdel*delW

            else
               delA = (hact-htop-0.5*epsdel)*delW
            endif
         else
            delA  = 0.
         endif
      else
         if (hact .gt. htopp) then
            delA = daext
         else if (hact .lt. hbase) then
            delA = 0.
         else
            epsdel = kwadr * ( htopp - hbase )
            delW = daext / (htopp - hbase) / ( 1.0 - kwadr )
            if ( hact .lt. hbase+epsdel) then
               delA = 0.5*(hact-hbase)*(hact-hbase)/epsdel*delW
               delW = (hact - hbase)/epsdel*delW

            else if ( hact .gt. htopp-epsdel) then
               delA = daext -&
               &0.5*(htopp-hact)*(htopp-hact)/epsdel*delW
               delW = (htopp-hact)/epsdel*delW

            else
               delA = (hact-hbase-0.5*epsdel)*delW
            endif
         endif
      endif
!
   else if (option .eq. 3) then
!
!        verouderde optie
!
      if (iter .eq. 1) then
         if (hact .gt. htop) artop = .false.
         if (hact .lt. hbase) artop = .true.
      endif
      if (artop) then
         if (hact .gt. htop) then
            delA = daext
         else
            delA = 0.
         endif
      else
         if (hact .lt. hbase) then
            delA = 0.
         else
            delA = daext
         endif
      endif
   endif
!
   if (artop) then
      iart = 1
   else
      iart = 0
   endif
!
end
