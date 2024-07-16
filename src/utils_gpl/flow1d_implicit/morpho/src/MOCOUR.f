      subroutine mocour ( nbran,   ngrid,
     +                    branch,  typcr,
     +                    grid,    celer,   x,
     +                    alphac,  dtm,     m, igpmc )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOCOUR (MORPHology COURant number)
c
c Module description: Determine maximum morphodynamic time step possi-
c                     ble.
c
c                     Determine maximum courant number followed by a
c                     calculation of the maximum morphodynamic time step
c                     possible. If the current time step is greater then
c                     the time step calculated return an integer M which
c                     will be used to reduce the time step of the morp-
c                     hology module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 alphac            I  Stability factor for bottom scheme (>1)
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c  9 dtm               I  Morphology time step.
c  5 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 10 m                 O  Number of times courant number > 1
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c  7 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mocour.pf,v $
c Revision 1.8  1999/03/15  15:52:46  kuipe_j
c tabs removed
c
c Revision 1.7  1996/05/28  13:30:05  kuipe_j
c Error message courant nr added
c
c Revision 1.6  1996/01/17  13:18:17  kuipe_j
c header update
c
c Revision 1.5  1995/11/21  11:08:59  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.4  1995/10/18  08:59:51  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:55:47  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:34  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:03  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:27  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  nbran,
     +         ngrid,
     +         m    ,igpmc

      integer  branch (4,nbran),
     +         typcr  (nbran)  ,
     +         grid   (ngrid)

      real     alphac,
     +         dtm

      real     celer  (ngrid,*),
     +         x      (ngrid)

c
c     Variables
c
      integer  ibr, i1, i2, igp, isec, nsec
      real     adcour, mcour, dx, avgcel
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Initialise maximum courant number
c
      mcour = 0.
c
      do 300 ibr = 1, nbran
c
c        Read first and last gridpoint of current branch
c
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
c
c        If sedredge branch check both sections !
c
         if (typcr(ibr) .eq. ccrsed) then
            nsec = 2
         else
            nsec = 1
         endif
c
         do 200 igp = i1, i2 - 1
c
c           Check for (structure grid)
c
            if (grid(igp).eq.1) then
c
c              Determine delta X
c
               dx = x(igp+1) - x(igp)

               do 100 isec = 1, nsec
c
c                 Determine courant number and maximum courant number
c
                  avgcel = .5 * (celer(igp,isec) + celer(igp+1,isec))
                  adcour = alphac * avgcel * dtm / dx
                  if (mcour .lt. abs(adcour))  then
                     mcour  = abs(adcour)
                     igpmc  = igp 
                  endif
 100           continue
            endif
 200     continue
 300  continue
c
c     Determine m (whole number)
c
      if (mcour .le. 1.) then
         m = 1
      else
         m = int (mcour + 1.0)
      endif
c
      return
      end
