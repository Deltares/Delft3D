subroutine setrfo (initra ,g      ,pacfac ,relden ,kinvis ,grn   ,&
&chezy  ,u      ,depth  ,hrad   ,uscofb ,trforb,&
&forcng ,sedtra )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SETRFO (SEdiment TRansport FOrmula)
!
! Module description: Select the appropriate transport formula and cal-
!                     culate the sediment transport in a grid point in a
!                     section.
!
!                     The appropriate module is called depending on the
!                     transport formula definition. After this the sedi-
!                     ment transport is multiplied with a calibration
!                     factor. The formula and calibration factor may be
!                     different for each branch.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 chezy             P  -
!  9 depth             P  -
! 13 forcng            P  -
!  2 g                 P  -
!  6 grn               P  -
! 10 hrad              P  -
!  1 initra            P  -
!  5 kinvis            P  -
!  3 pacfac            P  -
!  4 relden            P  -
! 14 sedtra            IO calculated sediment transport
! 12 trforb(3)         I  Defines for a branch a transport formula:
!                         (1) =   Transport formula number:
!                                 ctrfeh (1) : Engelund & Hansen
!                                 ctrfmm (2) : Meyer-Peter & Muller
!                                 ctrfaw (3) : Ackers & White
!                                 ctrfvr (4) : Van Rijn
!                                 ctrfpk (5) : Parker & Klingeman
!                                 ctrfud (6) : User Defined Formula
!                         (2) =   Starting index of the user coeffi-
!                                 cients for this branch.
!                         (3) =   Calibration factor for transport for-
!                                 mula.
!  8 u                 I  velocity
! 11 uscofb            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! setfaw  SEdiment Transport Formula Ackers & White
! setfeh  SEdiment Transp. Form. Engelund & Hansen
! setfmm  SEdiment Transp. Form. Meyer-Peter & Muller
! setfpk  SEdiment Transp. Form. Parker & Klingeman
! setfud  SEdiment Transport Formula User Defined
! setfvr  SEdiment Transport Formula Van Rijn
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: setrfo.pf,v $
! Revision 1.3  1996/12/03  08:24:17  kuipe_j
! calibration factor added in power distribution
!
! Revision 1.2  1995/05/30  07:07:41  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:36  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:11  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   real       g      ,pacfac   ,relden   ,kinvis  ,chezy  ,hrad  ,&
   &u      ,depth    ,sedtra
   real       grn(4) ,uscofb(*),forcng(4),trforb(3)
   logical    initra
!
!     Declaration of local parameters
!
   real               velo
!
!     Constants
!
   integer     d35   , d50   , d90   , dmed
   parameter  (d35=1 , d50=2 , d90=3 , dmed=4)
!
   velo  = abs(u)
!
   goto (10,20,30,40,50,60,70) int(trforb(1))
!
!        Engelund and Hansen
!
10 call setfeh&
   &(initra   ,g         ,pacfac  ,relden   ,grn(d50),chezy   ,&
   &velo     ,forcng(1) ,sedtra  )
   goto 100
!
!        Meyer-Peter and Muller
!
20 call setfmm&
   &(initra   ,g      ,pacfac  ,relden   ,grn(dmed),grn(d90) ,&
   &chezy    ,velo   ,hrad    ,forcng(1),sedtra   )
   goto 100
!
!        Ackers and White
!
30 call setfaw&
   &(initra   ,g        ,pacfac  ,relden ,kinvis   ,grn(d35) ,&
   &chezy    ,velo    ,depth  ,forcng(1),forcng(2),&
   &forcng(3),forcng(4),sedtra  )
   goto 100
!
!        Van Rijn
!
40 call setfvr&
   &(initra   ,g        ,pacfac ,relden  ,kinvis   ,grn(d50) ,&
   &grn(d90) ,velo     ,depth  ,hrad    ,forcng(1),forcng(2),&
   &forcng(3),forcng(4),sedtra )
   goto 100
!
!        Parker and Klingeman
!
50 call setfpk&
   &(initra   ,g        ,pacfac   ,relden ,grn(d50) ,chezy ,&
   &velo     ,forcng(1),forcng(2),sedtra )
   goto 100
!
!        User defined formula
!
60 call setfud&
   &(initra  ,g      ,pacfac ,relden ,grn(d50) ,grn(d90) ,&
   &chezy   ,uscofb ,velo   ,hrad   ,forcng(1),forcng(2),&
   &sedtra  )
   goto 100
!
!        No formula known, exit with transport = 0
!
70 sedtra = 0.
100 continue
!
   if (.not.initra) sedtra = sign(sedtra,u) * trforb(3)
!
end
