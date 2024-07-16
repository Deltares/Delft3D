      subroutine salt (nbran  ,nnode  ,nboun  ,nmouth ,
     &                 nstru  ,nhstat ,nqfloc ,nqlat  ,nslat  ,
     &                 ngrid  ,ngridm ,maxtab ,ntabm  ,ntmpgr ,
     &                 itim   ,dsopt  ,juer   ,g      ,
     &                 time   ,dt     ,theta  ,psi    ,tp     ,
     &                 branch ,mouth  ,bramrl ,node   ,indx   ,
     &                 strtyp ,hbdpar ,dispf  ,ntab   ,
     &                 grid   ,x      ,rho    ,disgr  ,abcd1  ,
     &                 abcd2  ,cp     ,qp     ,waoft  ,csa    ,
     &                 csd    ,rfv1   ,rfv2   ,tmpgr  ,cdcdx  ,
     &                 tw     ,thcsum ,sbdpar ,sbdscr ,emppar ,
     &                 mouqpu ,timout ,qfloq  ,salstr ,thasca ,
     &                 sltpar ,qltpar ,qlat   ,table  ,mat    ,
     &                 rhsvv  ,strclo ,strhis ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SALT (SALT module)
c
c Module description: Calculate salt transport in hydrodynamic network.
c
c                     The first step of the salt module is the calcula-
c                     tion of tidal parameters in case the chosen dis-
c                     persion formulation is the Thatcher-Harleman or
c                     Zwendl formulation. The following step is the
c                     calculation of dispersion coefficients for each
c                     grid point in the network (SADSPC). To build an
c                     abcde coefficient matrix lateral discharges as
c                     well as loads need to be known. Calculation of
c                     these parameters will be done in routine SALATS.
c                     In a later stage also the inflowing concentrations
c                     at boundaries have to be known. This calculation
c                     is performed by routine SABOUN. At this point all
c                     variables are known so the abcde coefficients are
c                     calculated and double sweeped by routine SADSCO.
c                     Now a nodal administration matrix will be build
c                     and solved. The solution of this matrix is used
c                     together with the double sweeped coefficients to
c                     calculate new salt concentration values in the
c                     network (SASOEQ). It is possible that the new
c                     calculated concentrations contain negative values.
c                     Therefore a filter will be applied in subroutine
c                     SAFILT. The last step is the calculation of the
c                     water density. This quantity must be passed to the
c                     water flow module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 40 abcd1             P  -
c 41 abcd2             P  -
c 28 bramrl            P  -
c 26 branch            P  -
c 50 cdcdx             P  -
c 42 cp                P  -
c 45 csa               P  -
c 46 csd               P  -
c 39 disgr             P  -
c 34 dispf             P  -
c 17 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c 22 dt                P  -
c 55 emppar            P  -
c 20 g                 P  -
c 36 grid              P  -
c 33 hbdpar            P  -
c 30 indx              P  -
c 16 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 19 juer              P  -
c 68 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 65 mat               P  -
c 18 maxfil            P  -
c 13 maxtab            I  Maximum number of defined tables.
c 56 mouqpu            P  -
c 27 mouth             P  -
c  4 nboun             I  Number of boundary nodes.
c  1 nbran             I  Number of branches.
c 11 ngrid             I  Number of grid points in network.
c 12 ngridm            I  Maximum number of gridpoints in a branch.
c  7 nhstat            I  Number of H-boundary stations.
c  5 nmouth            I  Maximum number of mouths in the network.
c  2 nnode             I  Number of nodes.
c 29 node              P  -
c  8 nqfloc            P  -
c  9 nqlat             P  -
c 10 nslat             P  -
c  6 nstru             P  -
c 35 ntab              P  -
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 15 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 24 psi               P  -
c 58 qfloq             P  -
c 63 qlat              P  -
c 62 qltpar            P  -
c 43 qp                P  -
c 47 rfv1              P  -
c 48 rfv2              P  -
c 38 rho               P  -
c 66 rhsvv             P  -
c 59 salstr            P  -
c 53 sbdpar            P  -
c 54 sbdscr            P  -
c 61 sltpar            P  -
c 67 strclo            P  -
c 32 strtyp            P  -
c 64 table             P  -
c 60 thasca            P  -
c 52 thcsum            P  -
c 23 theta             P  -
c 21 time              P  -
c 57 timout            P  -
c 49 tmpgr             P  -
c 25 tp                P  -
c 51 tw                P  -
c 44 waoft             P  -
c 37 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c saboun  SAlt BOUNdaries
c sadens  SAlt DENSity
c sadsco  SAlt Double Sweep COefficients
c sadspc  SAlt DiSPersion Coefficient
c safilt  SAlt FILTer
c saints  SAlt INItialise
c salats  SAlt LATeral Salt
c sasoeq  SAlt SOlve EQuations
c satidi  SAlt calculate TIDal Information
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: salt.pf,v $
c Revision 1.9  1999/03/15  15:53:24  kuipe_j
c tabs removed
c
c Revision 1.8  1997/11/26  14:44:48  kuipe_j
c diffusion zero for free flow
c
c Revision 1.7  1996/04/11  08:25:21  kuipe_j
c Kalman module added
c
c Revision 1.6  1995/10/18  09:00:26  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/08/30  12:37:20  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:43  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:13  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:10  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:52  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:29:33  kuipe_j
c Improved message handling
c
c Revision 1.3  1994/11/28  09:17:18  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:54  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer  nbran ,nnode  ,nboun ,nmouth ,nstru ,nhstat,
     &         nqfloc,nqlat  ,nslat  ,ngrid ,ngridm ,maxtab,ntabm ,
     &         ntmpgr,dsopt  ,juer  ,ker
      integer  branch(4,nbran)       ,grid  (ngrid)   ,
     &         mouth (2,*)           ,
     &         bramrl(nmouth+1,nbran),
     &         node  (4,nnode)       ,indx  (nnode)   ,
     &         strtyp(10,*)          ,
     &         hbdpar(3,nhstat)      ,
     &         dispf (2,3)           ,itim  (2)       ,
     &         ntab  (4,maxtab)
c
      real     g     ,theta    ,psi
      real     x     (ngrid)   ,rho   (ngrid)    ,disgr (ngrid)   ,
     &         cp    (ngrid,4) ,waoft (ngrid,6) ,
     &         csa   (ngrid,2) ,csd   (ngrid,2)  ,
     &         cdcdx (ngrid,3) ,tmpgr (ngrid,ntmpgr),
     &         tw    (nbran)   ,thcsum(2,nbran)  ,
     &         sbdpar(5,nboun) ,sbdscr(3,nboun)  ,
     &         emppar(4,*)     ,mouqpu(3,0:2,*)  ,timout(2,*)     ,
     &         qfloq (2,*)     ,
     &         salstr(7,nstru) ,strhis(dmstrh,nstru),
     &         thasca(3)       ,
     &         sltpar(9,*)     ,
     &         qltpar(9,*)     ,qlat  (*),
     &         table (ntabm)
c
      double precision  time   ,dt    ,tp
      double precision
     &         qp    (ngrid,3)        ,
     &         mat   (nnode,nnode)    ,rhsvv (nnode,2)    ,
     &         abcd1 (ngridm,5)       ,abcd2 (ngridm,5)   ,
     &         rfv1  (ngrid,3)        ,rfv2  (ngrid,3)

c
      logical  strclo(*)
c
c     Declaration of local parameters
c
      character txt*18
c
      ker = ok
c                          <csa1>   <csa2>   <csd1>   <csd2>
      call saints(ngrid   ,csa(1,1),csa(1,2),csd(1,1),csd(1,2))
c
      if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp)
     &call satidi(nmouth  ,nbran   ,nnode   ,nhstat  ,nqfloc  ,maxtab  ,
     &            ntabm   ,ngrid   ,dsopt   ,juer    ,time    ,dt      ,
     &            tp      ,mouth   ,branch  ,node    ,bramrl  ,hbdpar  ,
     &            ntab    ,table   ,grid    ,x       ,
c                 <q1>              <q2>              <af>
     &            qp(1,1)          ,qp(1,3)          ,waoft(1,3)       ,
c                 <csa1>            <dcdx>            <cdcdx0>
     &            csa(1,1)         ,tmpgr(1,3)       ,cdcdx(1,1)       ,
c                 <cdcdx1>          <cdcdx2>
     &            cdcdx(1,2)       ,cdcdx(1,3)       ,thasca           ,
     &            qfloq   ,emppar  ,mouqpu  ,thcsum  ,timout  ,ker     )
c
      if (ker .eq. fatal) goto 1000
c
      call sadspc(dsopt   ,nbran   ,ngrid   ,maxtab  ,ntabm   ,g       ,
     &            time    ,dispf   ,branch  ,ntab    ,table   ,thcsum  ,
c                                   <wf>              <q2>
     &            grid    ,x       ,waoft(1,1)       ,qp(1,3)          ,
c                 <c>               <csa1>            <cdcdx1>
     &            cp(1,1)          ,csa(1,1)         ,cdcdx(1,2)       ,
c                 <cdcdx2>          <distmp>
     &            cdcdx(1,3)       ,tmpgr(1,3)       ,disgr            )
c
      call salats(ngrid   ,nqlat   ,nslat   ,maxtab  ,ntabm   ,time    ,
c                                                     <csa1>   <csa2>
     &            dt      ,theta   ,psi     ,x       ,csa(1,1),csa(1,2),
c                                                              <source>
     &            qltpar  ,sltpar  ,qlat    ,ntab    ,table ,tmpgr(1,1),
c                 <qltgim>
     &            tmpgr(1,2)       )
c
      call saboun(nboun   ,nbran   ,ngrid   ,maxtab  ,ntabm   ,time    ,
c                 <q2>     <csa1>
     &            qp(1,3) ,csa(1,1),sbdpar  ,branch  ,ntab    ,table   ,
     &            sbdscr  )
c
      call sadsco(ngrid   ,ngridm  ,nbran   ,nstru   ,dt      ,psi     ,
c                          <q1>     <q2>     <qltgim>          <csa1>
     &            theta   ,qp(1,1) ,qp(1,3) ,tmpgr(1,2)       ,csa(1,1),
c                 <csd1>            <source>
     &            csd(1,1)         ,tmpgr(1,1)       ,disgr   ,x       ,
c                 <at1>             <at2>             <af>
     &            waoft(1,5)       ,waoft(1,4)       ,waoft(1,3)       ,
     &            branch  ,strtyp  ,salstr  ,strclo  ,strhis  ,
c                 <aa>              <ba>              <da>
     &            abcd1(1,1)       ,abcd1(1,2)       ,abcd1(1,3)       ,
c                 <ea>              <fd>              <gd>
     &            abcd1(1,4)       ,abcd2(1,1)       ,abcd2(1,2)       ,
c                 <md>              <nd>              <ra>
     &            abcd2(1,3)       ,abcd2(1,4)       ,abcd1(1,5)       ,
c                 <rd>
     &            abcd2(1,5)       ,
c                 <r1>              <f1>              <v1>
     &            rfv1(1,1)        ,rfv1(1,2)        ,rfv1(1,3)        ,
c                 <r2>              <f2>              <v2>
     &            rfv2(1,1)        ,rfv2(1,2)        ,rfv2(1,3)        )
c
      call sasoeq(nnode   ,nboun   ,nbran   ,ngrid   ,juer    ,node    ,
c                          <q2>
     &            branch  ,qp(1,3) ,sbdpar  ,sbdscr  ,rfv1    ,rfv2    ,
c                                   <rhs>
     &            mat              ,rhsvv(1,1)       ,indx    ,
c                 <vv>              <csa2>            <csd2>
     &            rhsvv(1,2)       ,csa(1,2)         ,csd(1,2)         ,
     &            ker     )
c
      if (ker .eq. fatal) goto 1000
c
      if (ker .eq. fatal) goto 1000
c
      call safilt(nbran   ,ngrid   ,branch  ,grid      ,x       ,
c                 <af>                       <filc>
     &            waoft(1,3)       ,disgr   ,tmpgr(1,3),
c                 <csd2>   <csa2>
     &            csd(1,2),csa(1,2))
c
c                                                        <csa2>
      call sadens(nbran   ,ngrid   ,juer  ,branch ,tw   ,csa(1,2),
     &             rho   ,ker   )
c
 1000 continue
c
      if (ker .ne. ok) then
         write (txt,'(2(1x,i8))') itim
         call error (juer,'SALT timestep@'//txt//'@',esames,info)
         if (ker .ne. fatal) ker = ok
      endif
c
      end
