!u    subroutine seds1b (nbran  ,ngrid  ,maxtab ,ntabm ,stot   ,bgout1 ,
!i3
subroutine seds1b (nbran  ,ngrid  ,maxtab ,ntabm ,stotfr ,bgout1 ,&
&sedinf ,sdrdbf ,ntab   ,q2    ,table  ,dissed ,&
&nfrac  ,nfracb )
!u   &                   sedinf ,sdrdbf ,ntab   ,q2    ,table  ,dissed )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDS1B (SEdiment Distribute Sediment for 1 Branch)
!
! Module description: Distribute sediment transport for one outflowing
!                     branch.
!
!                     The total sediment transport is stored in the
!                     outflowing branch if it is a normal branch. In
!                     case the branch is a Sedredge branch routine SEDI-
!                     SE is called. Routine SEDISE distributes the sedi-
!                     ment transport over the left and right channel.
!
! Precondition:       The total incoming transport in node is positive.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 bgout1(3)         I  Contains info for outflowing branch:
!                         (1) =   Branch number
!                         (2) =   Grid point number
!                         (3) =   Direction in branch:
!                                 +1 : First grid point is in node.
!                                 -1 : Last grid point is in node.
! 12 dissed(4,nbran)   O  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
!  3 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  9 ntab              P  -
!  4 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 10 q2                P  -
!  8 sdrdbf            P  -
!  7 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
!  5 stot              I  Total sediment transport going towards node.
! 11 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! sedise  SEdiment DIstribute SEdredge
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: seds1b.pf,v $
! Revision 1.2  1995/05/30  07:07:20  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:21  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:47  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
!i1
   integer    nbran ,ngrid     ,maxtab  ,ntabm  ,nfrac ,nfracb
!u    integer    nbran ,ngrid     ,maxtab  ,ntabm
   integer    sedinf(2,nbran)  ,sdrdbf(2,*) ,&
   &ntab  (4,maxtab) ,bgout1(3)
!u    real       stot
!u    real       q2    (ngrid)    ,table (ntabm)    ,dissed(4,nbran)
!i2
   real       table (ntabm)    ,stotfr(nfrac)  ,&
   &dissed(nfracb,2,nbran)

   double precision q2(ngrid)
!
!     Declaration of local parameters
!
!i1
   integer    ibr    ,igr    ,dir  ,sedbr ,iside, ifrac
!u    integer    ibr    ,igr    ,dir  ,sedbr
   real       s
   real       dissdb (2)
!
   ibr = bgout1(1)
   igr = bgout1(2)
   dir = bgout1(3)
!
!i5
   iside             = (3-dir)/2
   do 10 ifrac = 1,nfrac
      s                       = stotfr(ifrac) * real(dir)
      dissed(ifrac,iside,ibr) = s
10 continue
!u    s                 = stot * real(dir)
!u    dissed(2-dir,ibr) = s
!
!     Check for sedredge branches and if nessecary distribute sediment
!     over both channels.
!
!     Get sedredge branch number
!
   sedbr = sedinf(1,ibr)
   if (sedbr .ne. 0) then
!
!        Get total sediment and total discharge of the sedredge
!        branch.
!
      call sedise (sedbr  ,maxtab ,ntabm  ,s    ,q2(igr),&
      &sdrdbf ,ntab   ,table  ,dissdb )
!
!i2
      dissed(1,iside,ibr) = dissdb(1)
      dissed(2,iside,ibr) = dissdb(2)
!u       dissed(2-dir,ibr) = dissdb(1)
!u       dissed(3-dir,ibr) = dissdb(2)
!
   endif
!
end
