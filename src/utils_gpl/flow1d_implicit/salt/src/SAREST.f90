subroutine sarest (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,&
&nmouth ,ngrid  ,dsopt  ,ncelst ,nameel ,csa2  ,&
&csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca ,mouqpu,&
&thcsum ,timout ,sbdscr ,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAREST (SAlt REading of reSTart information)
!
! Module description: Reading  of restart information.
!
!                     The group definition depends a.o. on the selected
!                     dispersion formulation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 cdcdx0            P  -
! 15 cdcdx1            P  -
! 16 cdcdx2            P  -
! 12 csa2              P  -
! 13 csd2              P  -
!  2 dafdst            P  -
!  1 defdst            P  -
!  9 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  3 grnams            P  -
! 18 mouqpu            P  -
! 11 nameel            P  -
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
! 10 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 22 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  8 ngrid             I  Number of grid points in network.
!  7 nmouth            I  Maximum number of mouths in the network.
! 21 sbdscr            P  -
! 17 thasca            P  -
! 19 thcsum            P  -
! 20 timout            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getrel  GET Real ELement from a nefis file
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
! $Log: sarest.pf,v $
! Revision 1.2  1995/05/30  07:06:12  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:54  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:56  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:15  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri    ,nbran    ,nboun  ,nmouth ,ngrid  ,dsopt ,&
   &ncelst    ,neferr
   integer       fd_nefis_rst
   real          csa2  (ngrid)    ,csd2  (ngrid) ,&
   &cdcdx0(ngrid)    ,cdcdx1(ngrid)   ,cdcdx2(ngrid) ,&
   &thasca(3    )    ,mouqpu(3,0:2,*) ,&
   &thcsum(2,nbran)  ,timout(2,*)     ,sbdscr(3,nboun)
   character*(*) grnams
   character*(*) nameel(nentri)
!
!     Declaration of local variables
!
   integer       error     ,buflen
   integer       uindex(3) ,usrord(1)
!
!     Declaration of external functions
!
   integer       getrel
   external      getrel
!
   data          usrord    /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   buflen    = ngrid*4
!
   error = getrel (fd_nefis_rst, grnams ,nameel(2) ,&
   &uindex  ,usrord ,buflen ,csa2   )
   if (error.ne.0) goto 1000
!
   error = getrel (fd_nefis_rst, grnams ,nameel(3) ,&
   &uindex  ,usrord ,buflen ,csd2   )
   if (error.ne.0) goto 1000
!
   if (dsopt.eq.3 .or. dsopt.eq.4) then
!
      error = getrel (fd_nefis_rst, grnams ,nameel(4) ,&
      &uindex  ,usrord ,buflen ,cdcdx0  )
      if (error.ne.0) goto 1000
!
      error = getrel (fd_nefis_rst, grnams ,nameel(5) ,&
      &uindex  ,usrord ,buflen ,cdcdx1  )
      if (error.ne.0) goto 1000
!
      error = getrel (fd_nefis_rst, grnams ,nameel(6) ,&
      &uindex  ,usrord ,buflen ,cdcdx2  )
      if (error.ne.0) goto 1000
!
      buflen = nbran*2*4
      error  = getrel (fd_nefis_rst, grnams ,nameel(7) ,&
      &uindex  ,usrord ,buflen ,thcsum  )
      if (error.ne.0) goto 1000
!
      buflen = nmouth*3*3*4
      error  = getrel (fd_nefis_rst, grnams ,nameel(8) ,&
      &uindex  ,usrord ,buflen ,mouqpu  )
      if (error.ne.0) goto 1000
!
      buflen = nmouth*2*4
      error  = getrel (fd_nefis_rst, grnams ,nameel(9) ,&
      &uindex  ,usrord ,buflen ,timout  )
      if (error.ne.0) goto 1000
!
      buflen = 3*4
      error  = getrel (fd_nefis_rst, grnams ,nameel(10) ,&
      &uindex  ,usrord ,buflen ,thasca   )
      if (error.ne.0) goto 1000
!
   endif
!
   if (nboun.gt.0) then
      buflen = nboun*3*4
      error  = getrel (fd_nefis_rst, grnams ,nameel(11) ,&
      &uindex  ,usrord ,buflen ,sbdscr   )
      if (error.ne.0) goto 1000
   endif
!
1000 continue
   neferr = error
end
