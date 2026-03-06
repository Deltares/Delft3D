subroutine mocour ( nbran,   ngrid,&
&branch,  typcr,&
&grid,    celer,   x,&
&alphac,  dtm,     m, igpmc )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOCOUR (MORPHology COURant number)
!
! Module description: Determine maximum morphodynamic time step possi-
!                     ble.
!
!                     Determine maximum courant number followed by a
!                     calculation of the maximum morphodynamic time step
!                     possible. If the current time step is greater then
!                     the time step calculated return an integer M which
!                     will be used to reduce the time step of the morp-
!                     hology module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 alphac            I  Stability factor for bottom scheme (>1)
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
!  9 dtm               I  Morphology time step.
!  5 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 10 m                 O  Number of times courant number > 1
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
!  7 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mocour.pf,v $
! Revision 1.8  1999/03/15  15:52:46  kuipe_j
! tabs removed
!
! Revision 1.7  1996/05/28  13:30:05  kuipe_j
! Error message courant nr added
!
! Revision 1.6  1996/01/17  13:18:17  kuipe_j
! header update
!
! Revision 1.5  1995/11/21  11:08:59  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.4  1995/10/18  08:59:51  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:47  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:34  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:03  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:27  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  nbran,&
   &ngrid,&
   &m    ,igpmc

   integer  branch (4,nbran),&
   &typcr  (nbran)  ,&
   &grid   (ngrid)

   real     alphac,&
   &dtm

   real     celer  (ngrid,*),&
   &x      (ngrid)

!
!     Variables
!
   integer  ibr, i1, i2, igp, isec, nsec
   real     adcour, mcour, dx, avgcel
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Initialise maximum courant number
!
   mcour = 0.
!
   do 300 ibr = 1, nbran
!
!        Read first and last gridpoint of current branch
!
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
!
!        If sedredge branch check both sections !
!
      if (typcr(ibr) .eq. ccrsed) then
         nsec = 2
      else
         nsec = 1
      endif
!
      do 200 igp = i1, i2 - 1
!
!           Check for (structure grid)
!
         if (grid(igp).eq.1) then
!
!              Determine delta X
!
            dx = x(igp+1) - x(igp)

            do 100 isec = 1, nsec
!
!                 Determine courant number and maximum courant number
!
               avgcel = .5 * (celer(igp,isec) + celer(igp+1,isec))
               adcour = alphac * avgcel * dtm / dx
               if (mcour .lt. abs(adcour))  then
                  mcour  = abs(adcour)
                  igpmc  = igp
               endif
100         continue
         endif
200   continue
300 continue
!
!     Determine m (whole number)
!
   if (mcour .le. 1.) then
      m = 1
   else
      m = int (mcour + 1.0)
   endif
!
   return
end
