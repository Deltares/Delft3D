      subroutine FLAREX (iter  ,option,hact  ,htop  ,hbase ,daext ,
     +                   overlp,delA  ,iart  ,delW)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAREX (FLow correction for ARea EXtra)
c
c Module description: The calculated flow/total area must be corrected
c                     with the extra area.
c
c                     Depending on the option the area is calculated in
c                     the concerning grid point. At the beginning of
c                     every time step (iter=1) "artop" is determined.
c                     For every iteration step the area is calculated.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 daext             I  Extra area (flow or total).
c  8 dela              O  Computed extra area depending on option.
c 10 delw              IO Computed extra width depending on option.
c  3 hact              I  Actual water level at gridpoint in branch.
c  5 hbase             I  base-level
c  4 htop              I  top-level
c  9 iart              IO Status-variable indicating rising or falling
c                         water level.
c  1 iter              I  Iteration step.
c  2 option            I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c  7 overlp            I  adaptation height.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flarex.pf,v $
c Revision 1.5  1999/03/15  15:49:25  kuipe_j
c tabs removed
c
c Revision 1.4  1997/05/26  07:38:51  kuipe_j
c linearizatiom summerdikes introduced
c
c Revision 1.3  1996/04/12  13:03:37  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  iter  , option,iart
      real     hact  , htop  ,hbase ,daext ,overlp,delA, delW
c
c     Declaration of local variable:
c
      logical          artop
      real             htopp, epsdel, kwadr
c
      htopp = htop + overlp
      artop = iart .eq. 1
      delW  = 0.0
      kwadr = 0.5
      epsdel= kwadr * overlp
c
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
                  delA = daext -
     +                   0.5*(htopp-hact)*(htopp-hact)/epsdel*delW
                  delW = (htopp-hact)/epsdel*delW

               else
                  delA = (hact-htop-0.5*epsdel)*delW
               endif
            else
               delA  = 0.
            endif
         endif
c
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
                  delA = daext -
     +                   0.5*(htopp-hact)*(htopp-hact)/epsdel*delW
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
                  delA = daext -
     +                   0.5*(htopp-hact)*(htopp-hact)/epsdel*delW
                  delW = (htopp-hact)/epsdel*delW

               else
                  delA = (hact-hbase-0.5*epsdel)*delW
               endif
            endif
         endif
c
      else if (option .eq. 3) then
c
c        verouderde optie
c
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
c
      if (artop) then
         iart = 1
      else
         iart = 0
      endif
c
      end
