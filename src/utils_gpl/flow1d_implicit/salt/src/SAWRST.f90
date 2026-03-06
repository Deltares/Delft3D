subroutine sawrst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,&
&ngrid  ,dsopt  ,ncelst ,itim   ,curtim ,nameel,&
&csa2   ,csd2   ,cdcdx0 ,cdcdx1 ,cdcdx2 ,thasca,&
&mouqpu ,thcsum ,nmouth ,timout ,sbdscr ,sbdpar,&
&neferr)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAWRST (SAlt WRiting of reSTart information)
!
! Module description: Writing  of restart information.
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
!  8 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  3 grnams            P  -
! 10 itim              P  -
! 10 curtim            P  -
! 18 mouqpu            P  -
! 11 nameel            P  -
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
!  9 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 22 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  7 ngrid             I  Number of grid points in network.
!  2 nmouth            I  Maximum number of mouths in the network.
! 21 sbdscr            P  -
! 21 sbdpar            P  -
! 17 thasca            P  -
! 19 thcsum            P  -
! 20 timout            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
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
! $Log: sawrst.pf,v $
! Revision 1.6  1999/03/15  15:53:29  kuipe_j
! tabs removed
!
! Revision 1.5  1996/12/04  12:00:40  kuipe_j
! declarations / undefined vars
!
! Revision 1.4  1996/12/02  15:31:44  kuipe_j
! Salt restart improved
!
! Revision 1.3  1995/05/30  09:56:20  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:25  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:06  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:15  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:17  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri    ,nbran    ,nboun  ,ngrid  ,dsopt ,&
   &ncelst    ,neferr   ,nmouth
   integer       fd_nefis_rst, itim(2)
   real          csa2  (ngrid)    ,csd2  (ngrid) ,&
   &cdcdx0(ngrid)    ,cdcdx1(ngrid)   ,cdcdx2(ngrid)  ,&
   &thasca(3    )    ,mouqpu(3,0:2,nmouth)   ,curtim  ,&
   &thcsum(2,nbran)  ,timout(2,nmouth),sbdscr(3,nboun),&
   &sbdpar(5,nboun)
   character*(*) grnams
   character*(*) nameel(nentri)
!
!     Declaration of local variables
!
   integer       error
   integer       uindex(3) ,usrord(1)
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Declaration of external functions
!
   integer       flsdat    ,putrel    ,putiel
   external      flsdat    ,putrel    ,putiel
!
   data          usrord    /1/
!
!     In SOBEK a restart simulation always starts with zero time.
!     Therefore typical times for the Thatcher Harleman boundary
!     condition and Thatcher Harleman dispersion formulae should be
!     corrected before storing them on the restart file.
!
   call sarlti (-curtim , nboun , sbdpar , sbdscr ,&
   &dsopt  , thasca, nmouth , timout             )
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   error   = putiel (fd_nefis_rst, grnams ,nameel(1) ,&
   &uindex  ,usrord ,itim   )
   if (error.ne.0) goto 1000
!
   error = putrel (fd_nefis_rst, grnams ,nameel(2) ,&
   &uindex  ,usrord ,csa2   )
   if (error.ne.0) goto 1000
!
   error = putrel (fd_nefis_rst, grnams ,nameel(3) ,&
   &uindex  ,usrord ,csd2   )
   if (error.ne.0) goto 1000
!
   if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
!
      error = putrel (fd_nefis_rst, grnams ,nameel(4) ,&
      &uindex  ,usrord ,cdcdx0 )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst, grnams ,nameel(5) ,&
      &uindex  ,usrord ,cdcdx1 )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst, grnams ,nameel(6) ,&
      &uindex  ,usrord ,cdcdx2 )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst, grnams ,nameel(7) ,&
      &uindex  ,usrord ,thcsum )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst, grnams ,nameel(8) ,&
      &uindex  ,usrord ,mouqpu )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst, grnams ,nameel(9) ,&
      &uindex  ,usrord ,timout )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst, grnams ,nameel(10) ,&
      &uindex  ,usrord ,thasca )
      if (error.ne.0) goto 1000
!
   endif
!
   if (nboun.gt.0) then
      error = putrel (fd_nefis_rst, grnams ,nameel(11) ,&
      &uindex  ,usrord ,sbdscr )
      if (error.ne.0) goto 1000
   endif
!
   error = flsdat(fd_nefis_rst)
!
1000 continue
!
!     During a simulation more restart times may be written.
!     So, a reset of corrected Thatcher Harleman times (see above)
!     is required.
!
   call sarlti (+curtim , nboun , sbdpar , sbdscr ,&
   &dsopt  , thasca, nmouth , timout            )

   neferr = error
end
