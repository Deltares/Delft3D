subroutine salt (nbran  ,nnode  ,nboun  ,nmouth ,&
&nstru  ,nhstat ,nqfloc ,nqlat  ,nslat  ,&
&ngrid  ,ngridm ,maxtab ,ntabm  ,ntmpgr ,&
&itim   ,dsopt  ,juer   ,g      ,&
&time   ,dt     ,theta  ,psi    ,tp     ,&
&branch ,mouth  ,bramrl ,node   ,indx   ,&
&strtyp ,hbdpar ,dispf  ,ntab   ,&
&grid   ,x      ,rho    ,disgr  ,abcd1  ,&
&abcd2  ,cp     ,qp     ,waoft  ,csa    ,&
&csd    ,rfv1   ,rfv2   ,tmpgr  ,cdcdx  ,&
&tw     ,thcsum ,sbdpar ,sbdscr ,emppar ,&
&mouqpu ,timout ,qfloq  ,salstr ,thasca ,&
&sltpar ,qltpar ,qlat   ,table  ,mat    ,&
&rhsvv  ,strclo ,strhis ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SALT (SALT module)
!
! Module description: Calculate salt transport in hydrodynamic network.
!
!                     The first step of the salt module is the calcula-
!                     tion of tidal parameters in case the chosen dis-
!                     persion formulation is the Thatcher-Harleman or
!                     Zwendl formulation. The following step is the
!                     calculation of dispersion coefficients for each
!                     grid point in the network (SADSPC). To build an
!                     abcde coefficient matrix lateral discharges as
!                     well as loads need to be known. Calculation of
!                     these parameters will be done in routine SALATS.
!                     In a later stage also the inflowing concentrations
!                     at boundaries have to be known. This calculation
!                     is performed by routine SABOUN. At this point all
!                     variables are known so the abcde coefficients are
!                     calculated and double sweeped by routine SADSCO.
!                     Now a nodal administration matrix will be build
!                     and solved. The solution of this matrix is used
!                     together with the double sweeped coefficients to
!                     calculate new salt concentration values in the
!                     network (SASOEQ). It is possible that the new
!                     calculated concentrations contain negative values.
!                     Therefore a filter will be applied in subroutine
!                     SAFILT. The last step is the calculation of the
!                     water density. This quantity must be passed to the
!                     water flow module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 40 abcd1             P  -
! 41 abcd2             P  -
! 28 bramrl            P  -
! 26 branch            P  -
! 50 cdcdx             P  -
! 42 cp                P  -
! 45 csa               P  -
! 46 csd               P  -
! 39 disgr             P  -
! 34 dispf             P  -
! 17 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
! 22 dt                P  -
! 55 emppar            P  -
! 20 g                 P  -
! 36 grid              P  -
! 33 hbdpar            P  -
! 30 indx              P  -
! 16 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 19 juer              P  -
! 68 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 65 mat               P  -
! 18 maxfil            P  -
! 13 maxtab            I  Maximum number of defined tables.
! 56 mouqpu            P  -
! 27 mouth             P  -
!  4 nboun             I  Number of boundary nodes.
!  1 nbran             I  Number of branches.
! 11 ngrid             I  Number of grid points in network.
! 12 ngridm            I  Maximum number of gridpoints in a branch.
!  7 nhstat            I  Number of H-boundary stations.
!  5 nmouth            I  Maximum number of mouths in the network.
!  2 nnode             I  Number of nodes.
! 29 node              P  -
!  8 nqfloc            P  -
!  9 nqlat             P  -
! 10 nslat             P  -
!  6 nstru             P  -
! 35 ntab              P  -
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 15 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 24 psi               P  -
! 58 qfloq             P  -
! 63 qlat              P  -
! 62 qltpar            P  -
! 43 qp                P  -
! 47 rfv1              P  -
! 48 rfv2              P  -
! 38 rho               P  -
! 66 rhsvv             P  -
! 59 salstr            P  -
! 53 sbdpar            P  -
! 54 sbdscr            P  -
! 61 sltpar            P  -
! 67 strclo            P  -
! 32 strtyp            P  -
! 64 table             P  -
! 60 thasca            P  -
! 52 thcsum            P  -
! 23 theta             P  -
! 21 time              P  -
! 57 timout            P  -
! 49 tmpgr             P  -
! 25 tp                P  -
! 51 tw                P  -
! 44 waoft             P  -
! 37 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! saboun  SAlt BOUNdaries
! sadens  SAlt DENSity
! sadsco  SAlt Double Sweep COefficients
! sadspc  SAlt DiSPersion Coefficient
! safilt  SAlt FILTer
! saints  SAlt INItialise
! salats  SAlt LATeral Salt
! sasoeq  SAlt SOlve EQuations
! satidi  SAlt calculate TIDal Information
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
! $Log: salt.pf,v $
! Revision 1.9  1999/03/15  15:53:24  kuipe_j
! tabs removed
!
! Revision 1.8  1997/11/26  14:44:48  kuipe_j
! diffusion zero for free flow
!
! Revision 1.7  1996/04/11  08:25:21  kuipe_j
! Kalman module added
!
! Revision 1.6  1995/10/18  09:00:26  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.5  1995/08/30  12:37:20  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:43  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:13  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:10  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:52  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:29:33  kuipe_j
! Improved message handling
!
! Revision 1.3  1994/11/28  09:17:18  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:54  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer  nbran ,nnode  ,nboun ,nmouth ,nstru ,nhstat,&
   &nqfloc,nqlat  ,nslat  ,ngrid ,ngridm ,maxtab,ntabm ,&
   &ntmpgr,dsopt  ,juer  ,ker
   integer  branch(4,nbran)       ,grid  (ngrid)   ,&
   &mouth (2,*)           ,&
   &bramrl(nmouth+1,nbran),&
   &node  (4,nnode)       ,indx  (nnode)   ,&
   &strtyp(10,*)          ,&
   &hbdpar(3,nhstat)      ,&
   &dispf (2,3)           ,itim  (2)       ,&
   &ntab  (4,maxtab)
!
   real     g     ,theta    ,psi
   real     x     (ngrid)   ,rho   (ngrid)    ,disgr (ngrid)   ,&
   &cp    (ngrid,4) ,waoft (ngrid,6) ,&
   &csa   (ngrid,2) ,csd   (ngrid,2)  ,&
   &cdcdx (ngrid,3) ,tmpgr (ngrid,ntmpgr),&
   &tw    (nbran)   ,thcsum(2,nbran)  ,&
   &sbdpar(5,nboun) ,sbdscr(3,nboun)  ,&
   &emppar(4,*)     ,mouqpu(3,0:2,*)  ,timout(2,*)     ,&
   &qfloq (2,*)     ,&
   &salstr(7,nstru) ,strhis(dmstrh,nstru),&
   &thasca(3)       ,&
   &sltpar(9,*)     ,&
   &qltpar(9,*)     ,qlat  (*),&
   &table (ntabm)
!
   double precision  time   ,dt    ,tp
   double precision&
   &qp    (ngrid,3)        ,&
   &mat   (nnode,nnode)    ,rhsvv (nnode,2)    ,&
   &abcd1 (ngridm,5)       ,abcd2 (ngridm,5)   ,&
   &rfv1  (ngrid,3)        ,rfv2  (ngrid,3)

!
   logical  strclo(*)
!
!     Declaration of local parameters
!
   character txt*18
!
   ker = ok
!                          <csa1>   <csa2>   <csd1>   <csd2>
   call saints(ngrid   ,csa(1,1),csa(1,2),csd(1,1),csd(1,2))
!
   if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp)&
   &call satidi(nmouth  ,nbran   ,nnode   ,nhstat  ,nqfloc  ,maxtab  ,&
   &ntabm   ,ngrid   ,dsopt   ,juer    ,time    ,dt      ,&
   &tp      ,mouth   ,branch  ,node    ,bramrl  ,hbdpar  ,&
   &ntab    ,table   ,grid    ,x       ,&
!                 <q1>              <q2>              <af>
   &qp(1,1)          ,qp(1,3)          ,waoft(1,3)       ,&
!                 <csa1>            <dcdx>            <cdcdx0>
   &csa(1,1)         ,tmpgr(1,3)       ,cdcdx(1,1)       ,&
!                 <cdcdx1>          <cdcdx2>
   &cdcdx(1,2)       ,cdcdx(1,3)       ,thasca           ,&
   &qfloq   ,emppar  ,mouqpu  ,thcsum  ,timout  ,ker     )
!
   if (ker .eq. fatal) goto 1000
!
   call sadspc(dsopt   ,nbran   ,ngrid   ,maxtab  ,ntabm   ,g       ,&
   &time    ,dispf   ,branch  ,ntab    ,table   ,thcsum  ,&
!                                   <wf>              <q2>
   &grid    ,x       ,waoft(1,1)       ,qp(1,3)          ,&
!                 <c>               <csa1>            <cdcdx1>
   &cp(1,1)          ,csa(1,1)         ,cdcdx(1,2)       ,&
!                 <cdcdx2>          <distmp>
   &cdcdx(1,3)       ,tmpgr(1,3)       ,disgr            )
!
   call salats(ngrid   ,nqlat   ,nslat   ,maxtab  ,ntabm   ,time    ,&
!                                                     <csa1>   <csa2>
   &dt      ,theta   ,psi     ,x       ,csa(1,1),csa(1,2),&
!                                                              <source>
   &qltpar  ,sltpar  ,qlat    ,ntab    ,table ,tmpgr(1,1),&
!                 <qltgim>
   &tmpgr(1,2)       )
!
   call saboun(nboun   ,nbran   ,ngrid   ,maxtab  ,ntabm   ,time    ,&
!                 <q2>     <csa1>
   &qp(1,3) ,csa(1,1),sbdpar  ,branch  ,ntab    ,table   ,&
   &sbdscr  )
!
   call sadsco(ngrid   ,ngridm  ,nbran   ,nstru   ,dt      ,psi     ,&
!                          <q1>     <q2>     <qltgim>          <csa1>
   &theta   ,qp(1,1) ,qp(1,3) ,tmpgr(1,2)       ,csa(1,1),&
!                 <csd1>            <source>
   &csd(1,1)         ,tmpgr(1,1)       ,disgr   ,x       ,&
!                 <at1>             <at2>             <af>
   &waoft(1,5)       ,waoft(1,4)       ,waoft(1,3)       ,&
   &branch  ,strtyp  ,salstr  ,strclo  ,strhis  ,&
!                 <aa>              <ba>              <da>
   &abcd1(1,1)       ,abcd1(1,2)       ,abcd1(1,3)       ,&
!                 <ea>              <fd>              <gd>
   &abcd1(1,4)       ,abcd2(1,1)       ,abcd2(1,2)       ,&
!                 <md>              <nd>              <ra>
   &abcd2(1,3)       ,abcd2(1,4)       ,abcd1(1,5)       ,&
!                 <rd>
   &abcd2(1,5)       ,&
!                 <r1>              <f1>              <v1>
   &rfv1(1,1)        ,rfv1(1,2)        ,rfv1(1,3)        ,&
!                 <r2>              <f2>              <v2>
   &rfv2(1,1)        ,rfv2(1,2)        ,rfv2(1,3)        )
!
   call sasoeq(nnode   ,nboun   ,nbran   ,ngrid   ,juer    ,node    ,&
!                          <q2>
   &branch  ,qp(1,3) ,sbdpar  ,sbdscr  ,rfv1    ,rfv2    ,&
!                                   <rhs>
   &mat              ,rhsvv(1,1)       ,indx    ,&
!                 <vv>              <csa2>            <csd2>
   &rhsvv(1,2)       ,csa(1,2)         ,csd(1,2)         ,&
   &ker     )
!
   if (ker .eq. fatal) goto 1000
!
   if (ker .eq. fatal) goto 1000
!
   call safilt(nbran   ,ngrid   ,branch  ,grid      ,x       ,&
!                 <af>                       <filc>
   &waoft(1,3)       ,disgr   ,tmpgr(1,3),&
!                 <csd2>   <csa2>
   &csd(1,2),csa(1,2))
!
!                                                        <csa2>
   call sadens(nbran   ,ngrid   ,juer  ,branch ,tw   ,csa(1,2),&
   &rho   ,ker   )
!
1000 continue
!
   if (ker .ne. ok) then
      write (txt,'(2(1x,i8))') itim
      call error (juer,'SALT timestep@'//txt//'@',esames,info)
      if (ker .ne. fatal) ker = ok
   endif
!
end
