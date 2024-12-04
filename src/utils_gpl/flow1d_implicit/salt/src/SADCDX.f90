subroutine sadcdx (nbran  ,ngrid  ,branch ,grid   ,csa1   ,x     ,&
&dcdx   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADCDX (SAlt DC/DX calculation)
!
! Module description: Calculate dc/dx in every grid point of the net-
!                     work.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  5 csa1(ngrid)       I  Salt concentration in every grid point at time
!                         t(n).
!  7 dcdx(ngrid)       O  Scratch array for dc/dx.
!  4 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  6 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sadcdx.pf,v $
! Revision 1.7  1999/03/15  15:53:20  kuipe_j
! tabs removed
!
! Revision 1.6  1997/09/30  09:31:07  kuipe_j
! avoid array overflow
!
! Revision 1.5  1995/10/18  09:00:16  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/30  12:37:14  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/08/23  14:29:38  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.2  1995/05/30  07:05:55  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:36  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nbran   ,ngrid
   integer branch(4,nbran)
   integer grid  (ngrid)
   real    csa1  (ngrid)    ,x     (ngrid)   ,dcdx  (ngrid)
!
!     Declaration of local variables
!
   integer   ibr  ,igr  ,igp ,igm ,i1 ,i2 ,i1p ,i2m
   real      dx   ,dxeps
!
   data      dxeps/.01/

   do 20 ibr = 1,nbran
!
!        Calculation per grid point. Calculation of dc/dx at
!        the branch ends differs from the calculation at
!        intermediate points.
!
      i1  = branch(3,ibr)
      i2  = branch(4,ibr)
      i1p = i1+1
      i2m = i2-1
!
      dx = x(i1p)-x(i1 )
      if (dx .lt. dxeps) then
         dcdx(i1) = 0.
      else
         dcdx(i1) = (csa1(i1p)-csa1(i1 )) / dx
      endif
!
      dx = x(i2 )-x(i2m)
      if (dx .lt. dxeps) then
         dcdx(i2) = 0.
      else
         dcdx(i2) = (csa1(i2 )-csa1(i2m)) / dx
      endif
!
      do 10 igr = i1p,i2m
         igp = igr+1
         igm = igr-1
         dcdx(igr) = (csa1(igp)-csa1(igm)) /&
         &(x(igp)-x(igm))
10    continue
      do 15 igr = i1p,i2m
         if (grid(igr).eq.2) then
            igp = igr+1
            igm = igr-1
            dcdx(igr) = (csa1(igr)-csa1(igm)) /&
            &(x(igr)-x(igm))
            if (igr.lt.i2m) then
               dcdx(igp) = (csa1(igp+1)-csa1(igp)) /&
               &(x(igp+1)-x(igp))
            endif
         endif
15    continue
20 continue
!
end
subroutine sadcfi (nbran  ,ngrid  ,branch ,grid   ,csa1   ,x     ,&
&dcdx   ,filc   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADCFI (SAlt DC/dx calculation on FIlterred points)
!
! Module description: Calculate dc/dx only in grid points of the net-
!                     work with had a negative concentration.
!                     Remark: this routine looks like SDCDX that
!                     calculates dc/dx in every grid point.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  5 csa1(ngrid)       I  Salt concentration in every grid point at time
!                         t(n).
!  7 dcdx(ngrid)       O  Scratch array for dc/dx.
!  8 filc(ngrid)       I  1  = negative concentration is filterred away
!                         -1 = calculated concentration is positive
!  4 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  6 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!
!     Declaration of parameters
!
   integer nbran   ,ngrid
   integer branch(4,nbran)
   integer grid  (ngrid)
   real    csa1  (ngrid)    ,x     (ngrid)   ,dcdx  (ngrid)
   real    filc  (ngrid)
!
!     Declaration of local variables
!
   integer   ibr  ,igr  ,igp ,igm ,i1 ,i2 ,i1p ,i2m
   real      dx   ,dxeps
!
   data      dxeps/.01/

   do ibr = 1,nbran
!
!        Calculation per grid point. Calculation of dc/dx at
!        the branch ends differs from the calculation at
!        intermediate points.
!
      i1  = branch(3,ibr)
      i2  = branch(4,ibr)
      i1p = i1+1
      i2m = i2-1
!        on first point of branch
      if (filc(i1).gt.0.) then
         dx = x(i1p)-x(i1 )
         if (dx .lt. dxeps) then
            dcdx(i1) = 0.
         else
            dcdx(i1) = (csa1(i1p)-csa1(i1 )) / dx
         endif
      endif
!
!        on last point of branch
      if (filc(i2).gt.0.) then
         dx = x(i2 )-x(i2m)
         if (dx .lt. dxeps) then
            dcdx(i2) = 0.
         else
            dcdx(i2) = (csa1(i2 )-csa1(i2m)) / dx
         endif
      endif
!
!        on points in between
      do igr = i1p,i2m
         if (filc(igr).gt.0.) then
            igp = igr+1
            igm = igr-1
            dcdx(igr) = (csa1(igp)-csa1(igm)) /&
            &(x(igp)-x(igm))
         endif
      enddo
!
!        on structure points
      do igr = i1p,i2m
         if (grid(igr).eq.2) then
            if (filc(igr).gt.0.) then
               igp = igr+1
               igm = igr-1
               dcdx(igr) = (csa1(igr)-csa1(igm)) /&
               &(x(igr)-x(igm))
               if (igr.lt.i2m) then
                  dcdx(igp) = (csa1(igp+1)-csa1(igp)) /&
                  &(x(igp+1)-x(igp))
               endif
            endif
         endif
      enddo
   enddo
!
end

