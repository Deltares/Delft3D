subroutine KAHYPA(nbran  ,ngrid  ,nnf    ,branch ,typcr  ,h1     ,&
&h      ,h2     ,maxlev ,nlev   ,hlev   ,wft    ,&
&aft    ,of     ,bfrict ,bfricp ,q1     ,q      ,&
&maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,&
&grsize ,engpar ,x      ,nexres ,exres  ,juer   ,&
&prslot ,waoft  ,af2    ,wf2    ,grid   ,c      ,&
&r      ,alfab  ,scifri ,pfa    ,afacc  ,oacc   ,&
&dwfdh  ,dcdh   ,drdh   ,dalfdh ,dcdq   ,eta    ,&
&detadh ,theta2 ,overlp ,arex   ,arexcn ,arexop ,&
&ker    ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAHYPA (KAlman HYdraulic PArameters)
!
! Module description: In subroutine KAHYPA several hydraulic parameters
!                     are computed, needed for the computation of deri-
!                     vatives used to determine the ABCDE coefficients.
!
!                     The following hydraulic parameters and derivatives
!                     are calculated:
!                                                          in routine
!                     flow area, flow width:                   KAAWWP
!                     wetted perimeter:                        KAAWWP
!                     derivative of wetted perimeter:          KABOCH
!                     derivative of bousinessq coefficient :   KABOCH
!                     derivatives of Chezy coefficient: KABOCH, KADCDQ
!                     extra resistance and derivative:         KAERES
!                     flow area, flow width for structures: KAAWST
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 33 af2               P  -
! 41 afacc             P  -
! 13 aft               P  -
! 38 alfab             P  -
! 52 arex              P  -
! 53 arexcn            P  -
! 54 arexop            P  -
! 16 bfricp            P  -
! 15 bfrict            P  -
!  4 branch            P  -
! 36 c                 P  -
! 46 dalfdh            P  -
! 44 dcdh              P  -
! 47 dcdq              P  -
! 49 detadh            P  -
! 45 drdh              P  -
! 43 dwfdh             P  -
! 26 engpar            P  -
! 48 eta               P  -
! 29 exres             P  -
! 35 grid              P  -
! 25 grsize            P  -
!  6 h1                P  -
!  8 h2                P  -
!  7 h                 P  -
! 11 hlev              P  -
! 30 juer              P  -
! 55 ker               P  -
!  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 19 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
! 28 nexres            P  -
!  2 ngrid             I  Number of grid points in network.
! 10 nlev              P  -
!  3 nnf               I  Number of uncertain bed friction parameters.
! 21 ntab              P  -
! 20 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 42 oacc              P  -
! 14 of                P  -
! 51 overlp            P  -
! 40 pfa               P  -
! 31 prslot            P  -
! 56 psltvr            P  -
! 17 q1                P  -
! 18 q                 P  -
! 37 r                 P  -
! 39 scifri            P  -
! 23 sectc             P  -
! 24 sectv             P  -
! 22 table             P  -
! 50 theta2            P  -
!  5 typcr             P  -
! 32 waoft             P  -
! 34 wf2               P  -
! 12 wft               P  -
! 27 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kaawst  KAlman Wetted Perimeter
! kaawwp  KAlman Areas Widths Wetted Perimeter
! kaboch  KAlman Boussinesq and Ch‚zy coefficient
! kadcdq  KAlman Derivative of Ch‚zy to Discharge Q
! kaeres  KAlman Extra RESistance
! kaflcp  Kalman Flow Chezy correction Parameter
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kahypa.pf,v $
! Revision 1.5  1997/06/17  11:23:45  kuipe_j
! Initialize vars
!
! Revision 1.4  1997/01/23  08:29:37  kuipe_j
! Make flow module robust
!
! Revision 1.3  1996/11/05  13:48:00  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.2  1996/04/12  13:05:00  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:36  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of Parameters:
!
   integer nbran, ngrid, nnf, maxlev, nexres, juer, ker,&
   &branch(4,nbran), bfrict(3,nbran),&
   &typcr(nbran),grid(ngrid),&
   &maxtab, ntabm, ntab(4,maxtab),&
   &nlev(ngrid), scifri(ngrid),&
   &arexcn(ngrid,2), arexop(2)
   real    theta2, overlp
   real    table(ntabm),&
   &af2(ngrid), wf2(ngrid),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &of (ngrid,maxlev),&
   &waoft(ngrid,dmwaof),&
   &c(ngrid,4), r(ngrid,4), alfab(ngrid),&
   &bfricp(6,ngrid),&
   &grsize(4,ngrid,*), engpar(9), exres(3,*),&
   &sectc(ngrid,3), sectv(ngrid,dmsecv),&
   &x(ngrid), arex(ngrid,4),&
   &prslot(3,nbran), pfa(nnf), psltvr(7,ngrid)
   real    afacc(ngrid), oacc(ngrid), dwfdh(ngrid),&
   &dcdh(ngrid), drdh(ngrid), dalfdh(ngrid),&
   &dcdq(ngrid), eta(ngrid), detadh(ngrid)
   double precision hlev(ngrid,maxlev), h1(ngrid), h(ngrid),&
   &h2(ngrid), q1(ngrid), q(ngrid)

!
   real       dh
   parameter (dh=.001)
!
!     Computation of 1. flow area        Af'(1:ngrid)
!                    2. wetted perimeter O' (1:ngrid)
!                    3. dWf/dh on time n+1/2
!
   call KAAWWP (nbran  ,ngrid  ,branch ,typcr  ,prslot ,&
   &h1     ,h      ,maxlev ,nlev   ,hlev   ,&
   &wft    ,aft    ,of     ,juer   ,theta2 ,&
!                  <Wf>           <Af>
   &waoft(1,1)     ,waoft(1,3)     ,afacc  ,&
!                         <Wt>
   &oacc   ,dwfdh  ,overlp ,arex   ,arexcn ,&
   &arexop ,ker    ,psltvr )
!
!     Remove correction on total bottom friction
!
   call KAFLCP (ngrid  ,nnf    ,scifri ,pfa    ,c      )
!
!     Computation of 4. dC/dh on time n+theta
!                    5. dR/dh on time n+theta
!                    6. dALFAb/dh on time n+theta
!
   call KABOCH (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
   &h1     ,h      ,q1     ,q      ,theta2 ,0.0    ,&
   &maxtab ,ntabm  ,ntab   ,table  ,&
!                  <subsec>        <secth0>        <secth1>
   &sectc(1,1)     ,sectv(1,2)     ,sectv(1,3)     ,&
!                  <wfh0>          <wfh1>
   &sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,&
!                  <Af>            <O>             <Afh0>
   &waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,&
!                  <Afh1>          <Oh0>           <Oh1>
   &sectv(1,5)     ,sectv(1,6)     ,sectv(1,7)     ,&
!                                  <Asubsc>
   &prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,&
   &c(1,2) ,alfab  ,dcdh   ,drdh   ,dalfdh ,psltvr )
!
   call KABOCH (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
   &h1     ,h      ,q1     ,q      ,theta2 ,dh     ,&
   &maxtab ,ntabm  ,ntab   ,table  ,&
!                  <subsec>        <secth0>        <secth1>
   &sectc(1,1)     ,sectv(1,2)     ,sectv(1,3)     ,&
!                  <wfh0>          <wfh1>
   &sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,&
!                  <Af'>           <O'>            <Afh0>
   &afacc          ,oacc           ,sectv(1,4)     ,&
!                  <Afh1>          <Oh0>           <Oh1>
   &sectv(1,5)     ,sectv(1,6)     ,sectv(1,7)     ,&
!                                  <Asubsc>
   &prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,&
   &c(1,2) ,alfab  ,dcdh   ,drdh   ,dalfdh ,psltvr )
!
!     Computation of 7. dC/dQ on time n+theta
!
   call KADCDQ (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
   &h1     ,h      ,q1     ,q      ,&
   &maxtab ,ntabm  ,ntab   ,table  ,theta2 ,0.0    ,&
!                  <secth0>        <secth1>
   &sectv(1,2)     ,sectv(1,3)     ,&
!                  <wfh0>          <wfh1>
   &sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,&
!                  <Af>            <O>             <Afh0>
   &waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,&
!                  <Afh1>
   &sectv(1,5)     ,&
!                                  <Asubsc>
   &prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,&
   &c(1,2) ,r(1,2) ,dcdq   ,psltvr )
!
   call KADCDQ (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
   &h1     ,h      ,q1     ,q      ,&
   &maxtab ,ntabm  ,ntab   ,table  ,theta2 ,dh     ,&
!                  <secth0>        <secth1>
   &sectv(1,2)     ,sectv(1,3)     ,&
!                  <wfh0>          <wfh1>
   &sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,&
!                  <Af>            <O>             <Afh0>
   &waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,&
!                  <Afh1>
   &sectv(1,5)     ,&
!                                  <Asubsc>
   &prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,&
   &c(1,2) ,r(1,2) ,dcdq   ,psltvr )
!
!
!     Computation of 8. extra resistance eta for every gridpoint
!                    9. deta/dh on time n+1/2
!
   call KAERES (nbran  ,ngrid  ,branch ,h1     ,h      ,&
   &theta2 ,maxtab ,ntabm  ,ntab   ,table  ,&
   &x      ,nexres ,exres  ,eta    ,detadh )
!
!     Computation of 10. flow area and width for structures on t=n+1.
!
   call KAAWST (nbran  ,ngrid  ,branch ,typcr  ,prslot ,maxlev ,&
   &nlev   ,hlev   ,wft    ,aft    ,h2     ,wf2    ,&
   &af2    ,grid   ,overlp ,arex   ,arexcn ,arexop ,&
   &juer   ,ker    ,psltvr )

end
