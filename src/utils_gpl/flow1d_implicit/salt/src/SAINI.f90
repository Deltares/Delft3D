subroutine saini (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,&
&ntabm  ,maxtab ,nslat  ,ntmpgr ,itim   ,&
&juer   ,g      ,rhow   ,time   ,tp     ,&
&le     ,newres ,&
&dispf  ,mouth  ,branch ,bramrl ,ntab   ,&
&fd_nefis_rst, fd_nefis_new, table  ,&
&sbdpar ,sltpar ,moupar ,x      ,disgr  ,&
&csa    ,csd    ,cp     ,qp     ,waoft  ,&
&cdcdx  ,tmpgr  ,thasca ,rho    ,mouqpu ,&
&tw     ,thcsum ,timout ,sbdscr ,ncelsa ,&
&grid   ,ker    ,salini )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAINI (SAlt INItialise)
!
! Module description: Initialise the salt data arrays depending on the
!                     start option.
!
!                     Initialisation depends on if it is an initial run
!                     or a restart run.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 20 bramrl            P  -
! 19 branch            P  -
! 36 cdcdx             P  -
! 33 cp                P  -
! 31 csa               P  -
! 32 csd               P  -
! 25 dafdrn            P  -
! 23 dafdst            P  -
! 24 defdrn            P  -
! 22 defdst            P  -
! 30 disgr             P  -
! 17 dispf             P  -
!  1 dsopt             P  -
! 11 g                 P  -
! 46 grid              P  -
!  9 itim              P  -
! 10 juer              P  -
! 47 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 15 le                P  -
!  7 maxtab            I  Maximum number of defined tables.
! 28 moupar            P  -
! 40 mouqpu            P  -
! 18 mouth             P  -
!  3 nboun             I  Number of boundary nodes.
!  4 nbran             I  Number of branches.
! 45 ncelsa            P  -
! 16 newres            P  -
!  5 ngrid             I  Number of grid points in network.
!  2 nmouth            I  Maximum number of mouths in the network.
! 21 ntab              P  -
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  8 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 34 qp                P  -
! 39 rho               P  -
! 12 rhow              P  -
! 27 sbdpar            P  -
! 44 sbdscr            P  -
! 26 table             P  -
! 38 thasca            P  -
! 42 thcsum            P  -
! 13 time              P  -
! 43 timout            P  -
! 37 tmpgr             P  -
! 14 tp                P  -
! 41 tw                P  -
! 35 waoft             P  -
! 29 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! saini1  SAlt INItialise subroutine 1
! sarsta  SAlt read or write of ReSTArt information
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
! $Log: saini.pf,v $
! Revision 1.13  1999/07/27  15:08:57  kuipe_j
! improve restart
!
! Revision 1.12  1998/04/10  09:21:44  kuipe_j
! Error in call sadspc fixed
!
! Revision 1.11  1997/06/17  11:27:08  kuipe_j
! output in history format
!
! Revision 1.10  1997/02/17  10:27:47  kuipe_j
! Lateral  Q  in m3/s in cont equation
!
! Revision 1.9  1996/12/02  15:31:40  kuipe_j
! Salt restart improved
!
! Revision 1.8  1996/01/17  13:59:21  kuipe_j
! header update
!
! Revision 1.7  1996/01/16  15:01:43  kuipe_j
! Restart improvements
!
! Revision 1.6  1995/10/18  09:00:23  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.5  1995/08/30  12:37:18  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:41  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:10  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:05  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:47  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:12  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:44  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer dsopt ,nmouth   ,nboun     ,nbran ,ngrid ,maxtab   ,&
   &ntabm ,nslat    ,ntmpgr   ,juer      ,ker
   integer fd_nefis_rst, fd_nefis_new,&
   &ncelsa(3)       ,itim(2)   ,&
   &dispf (2,3)     ,mouth (2,*)      ,branch(4,nbran) ,&
   &bramrl(nmouth+1,nbran)            ,ntab(4,maxtab)  ,&
   &grid  (ngrid)   ,salini(*)
   real    g     ,rhow     ,le
   real    table (ntabm)   ,sbdpar(5,nboun)  ,moupar(5,*)     ,&
   &x     (ngrid)   ,disgr (ngrid)    ,&
   &cp    (ngrid,4) ,waoft (ngrid,6) ,&
   &csa   (ngrid,2) ,csd   (ngrid,2)  ,rho   (ngrid)   ,&
   &cdcdx (ngrid,3) ,tmpgr (ngrid,ntmpgr)  ,&
   &thasca(3      ) ,mouqpu(3,0:2,*)       ,&
   &thcsum(2,nbran) ,tw    (nbran)         ,&
   &timout(2,*)     ,sbdscr(3,nboun)       ,&
   &sltpar(9,nslat)
   double  precision  time , tp, qp(ngrid,3)
   logical newres
!
!     Declaration of local variables
!
   logical inires, first
   integer ifil
!
!     Include error code file
!
   include '..\include\errcod.i'
!
!     Set initial flags for HIS-files
!
   do 10 ifil=1,2
      salini(ifil)=0
10 continue
!
!     Check and update administration of lateral stations
!
   call CHLATA (ngrid  ,nslat ,nbran, sltpar ,grid  ,branch)
!
   ker = ok
   first = .true.
   call sarsta (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid   ,itim,&
   &0.0    ,juer   ,first  ,newres ,&
   &fd_nefis_rst, fd_nefis_new,&
!                  <csa2>          <csd2>
   &csa(1,2)       ,csd(1,2)       ,&
!                  <cdcdx0>        <cdcdx1>        <cdcdx2>
   &cdcdx(1,1)     ,cdcdx(1,2)     ,cdcdx(1,3)       ,&
   &thasca ,mouqpu ,thcsum ,&
!                                  <ncelst>
   &timout ,sbdscr ,sbdpar ,ncelsa(3) ,inires ,ker     )
   if (ker .ne. ok) goto 1000
!
!     Calculate density from concentrations
!                                                      <csa2>
   call sadens (nbran ,ngrid ,juer  ,branch ,tw   , csa(1,2)    ,&
   &rho   ,ker   )
!
   if (inires) then
!
!        Initialize in case of no restart data for salt module.
!
      call saini1 (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid   ,maxtab  ,&
      &ntabm  ,juer   ,g      ,rhow   ,time    ,tp      ,&
      &le     ,dispf  ,mouth  ,branch ,bramrl  ,ntab    ,&
      &table  ,sbdpar ,moupar ,grid   ,x       ,&
!                     <af>            <wf>
      &waoft(1,3)     ,waoft(1,1)     ,&
!                     <c>             <q2>            <csa2>
      &cp(1,1)        ,qp(1,3)        ,csa(1,2)         ,&
!                     <distmp>                        <csd2>
      &tmpgr(1,3)     ,disgr          ,csd(1,2),rho     ,&
!                     <cdcdx0>        <cdcdx1>        <cdcdx2>
      &cdcdx(1,1)     ,cdcdx(1,2)     ,cdcdx(1,3)       ,&
      &thasca ,mouqpu ,tw     ,thcsum ,timout ,sbdscr   ,&
      &ker    )
      if (ker .ne. ok) goto 1000
   endif
!
!     Calculate dispersion coefficient for output
!
   call sadspc (dsopt  ,nbran  ,ngrid  ,maxtab ,ntabm ,g      ,&
   &time   ,dispf  ,branch ,ntab   ,table ,thcsum ,&
!                                   <wf>        <q2>      <c>
   &grid   ,x      ,waoft(1,1) ,qp(1,3)   ,cp(1,1),&
!                  <csa2>           <cdcdx1>    <cdcdx2>  <temp>
   &csa(1,2)       ,cdcdx(1,2) ,cdcdx(1,3),csd(1,1),&
   &disgr                                           )
!
!
1000 continue
!
end
