subroutine FLA1M(ngrid  ,nbran  ,branch  ,typcr  ,&
&h1      ,h     ,maxlev  ,nlev   ,hlev   ,&
&wft    ,af     ,izwft   ,a1m    ,theta2 )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLA1M (FLow A1M)
!
! Module description: Compute first order momentum cross section A1m for
!                     each grid point in network.
!
!                     Notice that several formulations for cross secti-
!                     ons are possible. Each type will be processed by a
!                     different routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 a1m               P  -
! 12 af                P  -
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 h1                P  -
!  7 h                 P  -
! 10 hlev              P  -
! 13 izwft             P  -
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
!  9 nlev              P  -
! 15 theta2            P  -
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 11 wft               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! fla1mc  FLow A1M Circle
! fla1ms  FLow A1M Sedredge
! fla1mt  FLow A1M Table
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fla1m.pf,v $
! Revision 1.7  1997/09/30  09:25:21  kuipe_j
! density term improved for Preisman slot
!
! Revision 1.6  1997/01/23  08:28:51  kuipe_j
! Make flow module robust
!
! Revision 1.5  1995/10/18  08:59:12  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/12  08:10:39  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:30:18  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer   ngrid, nbran, maxlev
   integer   branch(4,nbran), typcr(nbran), nlev(ngrid)
   double precision      hlev(ngrid,maxlev)
   double precision      h1(ngrid), h(ngrid)
   real      wft  (ngrid,maxlev), af (ngrid)
   real      izwft(ngrid,maxlev), a1m(ngrid)
   real      theta2
!
!     Declaration of local variables:
!
   integer  ibr, i1, i2
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Loop over branches
!
   do 100 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
!        Normal branch with tabulated cross sections
!
      if ( typcr(ibr) .eq. ccrtab ) then
         call FLA1MT(i1     ,i2     ,ngrid  ,h1     ,&
         &h      ,maxlev ,nlev   ,hlev   ,wft    ,af     ,&
         &izwft  ,a1m    ,theta2 )
!
!        Circle cross section in branch
!
      else if ( typcr(ibr) .eq. ccrcir ) then
         call FLA1MC(i1     ,i2     ,ngrid  ,h1     ,h      ,maxlev ,&
         &hlev   ,wft    ,a1m   ,theta2)
!
!        Sedredge branch
!
      else if ( typcr(ibr) .eq. ccrsed ) then
         call FLA1MS(i1     ,i2     ,ngrid  ,h1     ,h      ,maxlev ,&
         &hlev   ,af     ,a1m    ,theta2 )
      endif
100 continue
!
end
