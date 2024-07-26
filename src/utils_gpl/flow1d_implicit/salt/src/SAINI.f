      subroutine saini (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,
     &                  ntabm  ,maxtab ,nslat  ,ntmpgr ,itim   ,
     &                  juer   ,g      ,rhow   ,time   ,tp     ,
     &                  le     ,newres ,
     &                  dispf  ,mouth  ,branch ,bramrl ,ntab   ,
     &                  fd_nefis_rst, fd_nefis_new, table  ,
     &                  sbdpar ,sltpar ,moupar ,x      ,disgr  ,
     &                  csa    ,csd    ,cp     ,qp     ,waoft  ,
     &                  cdcdx  ,tmpgr  ,thasca ,rho    ,mouqpu ,
     &                  tw     ,thcsum ,timout ,sbdscr ,ncelsa ,
     &                  grid   ,ker    ,salini )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAINI (SAlt INItialise)
c
c Module description: Initialise the salt data arrays depending on the
c                     start option.
c
c                     Initialisation depends on if it is an initial run
c                     or a restart run.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 20 bramrl            P  -
c 19 branch            P  -
c 36 cdcdx             P  -
c 33 cp                P  -
c 31 csa               P  -
c 32 csd               P  -
c 25 dafdrn            P  -
c 23 dafdst            P  -
c 24 defdrn            P  -
c 22 defdst            P  -
c 30 disgr             P  -
c 17 dispf             P  -
c  1 dsopt             P  -
c 11 g                 P  -
c 46 grid              P  -
c  9 itim              P  -
c 10 juer              P  -
c 47 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 15 le                P  -
c  7 maxtab            I  Maximum number of defined tables.
c 28 moupar            P  -
c 40 mouqpu            P  -
c 18 mouth             P  -
c  3 nboun             I  Number of boundary nodes.
c  4 nbran             I  Number of branches.
c 45 ncelsa            P  -
c 16 newres            P  -
c  5 ngrid             I  Number of grid points in network.
c  2 nmouth            I  Maximum number of mouths in the network.
c 21 ntab              P  -
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  8 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 34 qp                P  -
c 39 rho               P  -
c 12 rhow              P  -
c 27 sbdpar            P  -
c 44 sbdscr            P  -
c 26 table             P  -
c 38 thasca            P  -
c 42 thcsum            P  -
c 13 time              P  -
c 43 timout            P  -
c 37 tmpgr             P  -
c 14 tp                P  -
c 41 tw                P  -
c 35 waoft             P  -
c 29 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c saini1  SAlt INItialise subroutine 1
c sarsta  SAlt read or write of ReSTArt information
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
c $Log: saini.pf,v $
c Revision 1.13  1999/07/27  15:08:57  kuipe_j
c improve restart
c
c Revision 1.12  1998/04/10  09:21:44  kuipe_j
c Error in call sadspc fixed
c
c Revision 1.11  1997/06/17  11:27:08  kuipe_j
c output in history format
c
c Revision 1.10  1997/02/17  10:27:47  kuipe_j
c Lateral  Q  in m3/s in cont equation
c
c Revision 1.9  1996/12/02  15:31:40  kuipe_j
c Salt restart improved
c
c Revision 1.8  1996/01/17  13:59:21  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:43  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/10/18  09:00:23  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/08/30  12:37:18  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:41  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:10  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:05  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:47  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:12  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:44  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer dsopt ,nmouth   ,nboun     ,nbran ,ngrid ,maxtab   ,
     &        ntabm ,nslat    ,ntmpgr   ,juer      ,ker
      integer fd_nefis_rst, fd_nefis_new,
     &        ncelsa(3)       ,itim(2)   ,
     &        dispf (2,3)     ,mouth (2,*)      ,branch(4,nbran) ,
     &        bramrl(nmouth+1,nbran)            ,ntab(4,maxtab)  ,
     &        grid  (ngrid)   ,salini(*)
      real    g     ,rhow     ,le
      real    table (ntabm)   ,sbdpar(5,nboun)  ,moupar(5,*)     ,
     &        x     (ngrid)   ,disgr (ngrid)    ,
     &        cp    (ngrid,4) ,waoft (ngrid,6) ,
     &        csa   (ngrid,2) ,csd   (ngrid,2)  ,rho   (ngrid)   ,
     &        cdcdx (ngrid,3) ,tmpgr (ngrid,ntmpgr)  ,
     &        thasca(3      ) ,mouqpu(3,0:2,*)       ,
     &        thcsum(2,nbran) ,tw    (nbran)         ,
     &        timout(2,*)     ,sbdscr(3,nboun)       ,
     &        sltpar(9,nslat)
      double  precision  time , tp, qp(ngrid,3)
      logical newres
c
c     Declaration of local variables
c
      logical inires, first
      integer ifil
c
c     Include error code file
c
      include '..\include\errcod.i'
c
c     Set initial flags for HIS-files
c
      do 10 ifil=1,2
         salini(ifil)=0
   10 continue
c
c     Check and update administration of lateral stations
c
      call CHLATA (ngrid  ,nslat ,nbran, sltpar ,grid  ,branch)
c
      ker = ok
      first = .true.
      call sarsta (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid   ,itim,
     &             0.0    ,juer   ,first  ,newres ,
     &             fd_nefis_rst, fd_nefis_new, 
c                  <csa2>          <csd2>
     &             csa(1,2)       ,csd(1,2)       ,
c                  <cdcdx0>        <cdcdx1>        <cdcdx2>
     &             cdcdx(1,1)     ,cdcdx(1,2)     ,cdcdx(1,3)       ,
     &             thasca ,mouqpu ,thcsum ,
c                                  <ncelst>
     &             timout ,sbdscr ,sbdpar ,ncelsa(3) ,inires ,ker     )
      if (ker .ne. ok) goto 1000
c
c     Calculate density from concentrations
c                                                      <csa2>
      call sadens (nbran ,ngrid ,juer  ,branch ,tw   , csa(1,2)    ,
     &             rho   ,ker   )
c
      if (inires) then
c
c        Initialize in case of no restart data for salt module.
c
         call saini1 (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid   ,maxtab  ,
     &                ntabm  ,juer   ,g      ,rhow   ,time    ,tp      ,
     &                le     ,dispf  ,mouth  ,branch ,bramrl  ,ntab    ,
     &                table  ,sbdpar ,moupar ,grid   ,x       ,
c                     <af>            <wf>
     &                waoft(1,3)     ,waoft(1,1)     ,
c                     <c>             <q2>            <csa2>
     &                cp(1,1)        ,qp(1,3)        ,csa(1,2)         ,
c                     <distmp>                        <csd2>
     &                tmpgr(1,3)     ,disgr          ,csd(1,2),rho     ,
c                     <cdcdx0>        <cdcdx1>        <cdcdx2>
     &                cdcdx(1,1)     ,cdcdx(1,2)     ,cdcdx(1,3)       ,
     &                thasca ,mouqpu ,tw     ,thcsum ,timout ,sbdscr   ,
     &                ker    )
         if (ker .ne. ok) goto 1000
      endif
c
c     Calculate dispersion coefficient for output
c
      call sadspc (dsopt  ,nbran  ,ngrid  ,maxtab ,ntabm ,g      ,
     &             time   ,dispf  ,branch ,ntab   ,table ,thcsum ,
c                                   <wf>        <q2>      <c>
     &             grid   ,x      ,waoft(1,1) ,qp(1,3)   ,cp(1,1),
c                  <csa2>           <cdcdx1>    <cdcdx2>  <temp>
     &             csa(1,2)       ,cdcdx(1,2) ,cdcdx(1,3),csd(1,1),
     &             disgr                                           )
c
c
 1000 continue
c
      end
