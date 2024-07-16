      subroutine sadcdx (nbran  ,ngrid  ,branch ,grid   ,csa1   ,x     ,
     &                   dcdx   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADCDX (SAlt DC/DX calculation)
c
c Module description: Calculate dc/dx in every grid point of the net-
c                     work.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  5 csa1(ngrid)       I  Salt concentration in every grid point at time
c                         t(n).
c  7 dcdx(ngrid)       O  Scratch array for dc/dx.
c  4 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  6 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sadcdx.pf,v $
c Revision 1.7  1999/03/15  15:53:20  kuipe_j
c tabs removed
c
c Revision 1.6  1997/09/30  09:31:07  kuipe_j
c avoid array overflow
c
c Revision 1.5  1995/10/18  09:00:16  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/30  12:37:14  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/08/23  14:29:38  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.2  1995/05/30  07:05:55  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:36  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nbran   ,ngrid
      integer branch(4,nbran)
      integer grid  (ngrid)
      real    csa1  (ngrid)    ,x     (ngrid)   ,dcdx  (ngrid)
c
c     Declaration of local variables
c
      integer   ibr  ,igr  ,igp ,igm ,i1 ,i2 ,i1p ,i2m
      real      dx   ,dxeps
c
      data      dxeps/.01/

      do 20 ibr = 1,nbran
c
c        Calculation per grid point. Calculation of dc/dx at
c        the branch ends differs from the calculation at
c        intermediate points.
c
         i1  = branch(3,ibr)
         i2  = branch(4,ibr)
         i1p = i1+1
         i2m = i2-1
c
         dx = x(i1p)-x(i1 )
         if (dx .lt. dxeps) then
            dcdx(i1) = 0.
         else
            dcdx(i1) = (csa1(i1p)-csa1(i1 )) / dx
         endif
c
         dx = x(i2 )-x(i2m)
         if (dx .lt. dxeps) then
            dcdx(i2) = 0.
         else
            dcdx(i2) = (csa1(i2 )-csa1(i2m)) / dx
         endif
c
         do 10 igr = i1p,i2m
            igp = igr+1
            igm = igr-1
            dcdx(igr) = (csa1(igp)-csa1(igm)) /
     &                  (x(igp)-x(igm))
   10    continue
         do 15 igr = i1p,i2m
            if (grid(igr).eq.2) then
               igp = igr+1
               igm = igr-1
               dcdx(igr) = (csa1(igr)-csa1(igm)) /
     &                     (x(igr)-x(igm))
               if (igr.lt.i2m) then
                  dcdx(igp) = (csa1(igp+1)-csa1(igp)) /
     &                        (x(igp+1)-x(igp))
               endif
            endif
   15    continue
   20 continue
c
      end
      subroutine sadcfi (nbran  ,ngrid  ,branch ,grid   ,csa1   ,x     ,
     &                   dcdx   ,filc   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADCFI (SAlt DC/dx calculation on FIlterred points)
c
c Module description: Calculate dc/dx only in grid points of the net-
c                     work with had a negative concentration.
c                     Remark: this routine looks like SDCDX that 
c                     calculates dc/dx in every grid point.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  5 csa1(ngrid)       I  Salt concentration in every grid point at time
c                         t(n).
c  7 dcdx(ngrid)       O  Scratch array for dc/dx.
c  8 filc(ngrid)       I  1  = negative concentration is filterred away
c                         -1 = calculated concentration is positive
c  4 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  6 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c
c     Declaration of parameters
c
      integer nbran   ,ngrid
      integer branch(4,nbran)
      integer grid  (ngrid)
      real    csa1  (ngrid)    ,x     (ngrid)   ,dcdx  (ngrid)
      real    filc  (ngrid) 
c
c     Declaration of local variables
c
      integer   ibr  ,igr  ,igp ,igm ,i1 ,i2 ,i1p ,i2m
      real      dx   ,dxeps
c
      data      dxeps/.01/

      do ibr = 1,nbran
c
c        Calculation per grid point. Calculation of dc/dx at
c        the branch ends differs from the calculation at
c        intermediate points.
c
         i1  = branch(3,ibr)
         i2  = branch(4,ibr)
         i1p = i1+1
         i2m = i2-1
c        on first point of branch
         if (filc(i1).gt.0.) then
            dx = x(i1p)-x(i1 )
            if (dx .lt. dxeps) then
               dcdx(i1) = 0.
            else
               dcdx(i1) = (csa1(i1p)-csa1(i1 )) / dx
            endif
         endif
c
c        on last point of branch
         if (filc(i2).gt.0.) then
            dx = x(i2 )-x(i2m)
            if (dx .lt. dxeps) then
               dcdx(i2) = 0.
            else
               dcdx(i2) = (csa1(i2 )-csa1(i2m)) / dx
            endif
         endif
c
c        on points in between
         do igr = i1p,i2m
            if (filc(igr).gt.0.) then
               igp = igr+1
               igm = igr-1
               dcdx(igr) = (csa1(igp)-csa1(igm)) /
     &                     (x(igp)-x(igm))
            endif  
         enddo
c   
c        on structure points
         do igr = i1p,i2m
            if (grid(igr).eq.2) then
               if (filc(igr).gt.0.) then
                  igp = igr+1
                  igm = igr-1
                  dcdx(igr) = (csa1(igr)-csa1(igm)) /
     &                        (x(igr)-x(igm))
                  if (igr.lt.i2m) then
                     dcdx(igp) = (csa1(igp+1)-csa1(igp)) /
     &                           (x(igp+1)-x(igp))
                  endif
               endif
            endif
         enddo
      enddo
c
      end
       
