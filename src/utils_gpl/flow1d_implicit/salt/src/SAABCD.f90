subroutine saabcd (ngrid  ,ngridm ,nstru  ,i1     ,i2     ,dt    ,&
&psi    ,theta  ,q1     ,q2     ,qltgim ,csa1  ,&
&csd1   ,source ,disgr  ,x      ,at1    ,at2   ,&
&af     ,strtyp ,salstr ,strclo ,strhis ,&
&aa     ,ba     ,da     ,ea     ,fd     ,gd    ,&
&md     ,nd     ,ra     ,rd     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAABCD (SAlt A,B,(C),D,e-etc. coeff. calculation)
!
! Module description: Calculate the A,B,D,E-etc. coefficients for each
!                     gridpoint in a branch.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 23 aa                P  -
! 19 af                P  -
! 17 at1               P  -
! 18 at2               P  -
! 24 ba                P  -
! 12 csa1              P  -
! 13 csd1              P  -
! 25 da                P  -
! 15 disgr             P  -
!  6 dt                P  -
! 26 ea                P  -
! 27 fd(ngridm)        O  f-coefficient [cs(i)] in diffusion equation
!                         for every grid point in a branch.
! 28 gd                P  -
!  4 i1                I  Index of first grid point in actual branch.
!  5 i2                I  Index of last grid point in actual branch.
! 29 md                P  -
! 30 nd                P  -
!  1 ngrid             I  Number of grid points in network.
!  2 ngridm            I  Maximum number of gridpoints in a branch.
!  3 nstru             I  Number of structures.
!  7 psi               P  -
!  9 q1                P  -
! 10 q2                P  -
! 11 qltgim            P  -
! 31 ra                P  -
! 32 rd                P  -
! 21 salstr            P  -
! 14 source            P  -
! 22 strclo            P  -
! 20 strtyp(10,nstru)  I  Structure definitions:
!                         (1,i) = Type of structure:
!                                 csweir (1) : Simple weir
!                                 caweir (2) : Advanced weir
!                                 csgate (3) : - not used, reserved for
!                                                simple gate -
!                                 cpump  (4) : Pump
!                                 cgenst (5) : General Structure
!                         (2,i) = Position of structure:
!                                 cstbra (1) : In branch
!                                 cstlat (2) : Lateral structure
!                         (3,i) = Left gridpoint.
!                         (4,i) = Right gridpoint.
!                         (5,i) = dummy
!                         (6,i) = dummy
!                         (7,i) = dummy
!                         (8,i) = dummy
!                         (9,i) = dummy
!                         (10,i)= dummy
!  8 theta             P  -
! 16 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! sanorm  SAlt NORMal a,b,d,e-etc. coeff. calc.
! sastru  SAlt a,b,d,e-etc. coef. cal. STRUctures
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: saabcd.pf,v $
! Revision 1.7  1999/03/15  15:53:15  kuipe_j
! tabs removed
!
! Revision 1.6  1997/11/26  14:44:45  kuipe_j
! diffusion zero for free flow
!
! Revision 1.5  1995/10/18  09:00:11  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/23  14:29:35  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:01  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:50  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:31  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:16:59  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:18  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:11  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    ngrid  ,ngridm ,nstru  ,i1     ,i2
   real       psi    ,theta
   integer    strtyp(10,*)
   real       qltgim(ngrid)  ,&
   &csa1  (ngrid)  ,csd1  (ngrid)  ,source(ngrid)  ,&
   &disgr (ngrid)  ,x     (ngrid)  ,at1   (ngrid)  ,&
   &at2   (ngrid)  ,af    (ngrid)  ,&
   &salstr(7,nstru),strhis(dmstrh,nstru)
   double precision  dt
   double precision&
   &q1    (ngrid ) ,q2   (ngrid ) ,&
   &aa    (ngridm) ,ba   (ngridm) ,da    (ngridm) ,&
   &ea    (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,&
   &md    (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,&
   &rd    (ngridm)
   logical    strclo(nstru)
!
!     Declaration of local parameters
!
   integer    il    ,ir    ,j     ,istr
!
   call sanorm (ngrid  ,ngridm ,i1     ,i2     ,dt     ,psi   ,&
   &theta  ,q1     ,q2     ,qltgim ,csa1   ,csd1  ,&
   &source ,disgr  ,x      ,at1    ,at2    ,af    ,&
   &aa     ,ba     ,da     ,ea     ,fd     ,gd    ,&
   &md     ,nd     ,ra     ,rd      )
!
!     Make equations for structures that reside in this branch.
!     Initialize coefficient fd, ba and ea to detect flow type in
!     parallel structures.
!
   do 10 istr = 1 , nstru
      if (strtyp(2,istr) .eq. cstbra  .and.&
      &strtyp(3,istr) .ge. i1 .and. strtyp(3,istr) .le. i2 ) then
         il =  strtyp(3,istr)
         j  =  il - i1 + 1
         fd(j) = 0.d0
         ba(j) = 0.d0
         ea(j) = 0.d0
      endif
10 continue
!
   do 20 istr = 1 , nstru
      if (strtyp(2,istr) .eq. cstbra  .and.&
      &strtyp(3,istr) .ge. i1 .and. strtyp(3,istr) .le. i2 ) then
         il =  strtyp(3,istr)
         ir =  strtyp(4,istr)
         j  =  il - i1 + 1
!
         call sastru (ngrid ,ngridm ,il    ,ir     ,j      ,istr  ,&
         &q2     ,af    ,disgr ,salstr ,strclo ,strhis,&
         &aa     ,ba    ,da    ,ea     ,fd     ,gd    ,&
         &md     ,nd    ,ra    ,rd     )
!
      endif
20 continue

!JK   do 99 i=1,i2-i1
!JK   WRITE (99,*)  'ADVECT: aa,ba,da,ea,fd,gd',i+i1-1
!JK   WRITE (99,*)   aa(i)  ,ba(i)  ,da(i)  ,ea(i)   ,fd(i)  ,gd(i)
!JK   WRITE (99,*)  'DIFFUS:md,nd,ra,rd',i+i1-1
!JK   WRITE (99,*)   md(i)  ,nd(i)  ,ra(i)  ,rd(i )
!JK9  continue
end
