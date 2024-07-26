      subroutine sawres (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,
     &                   nsamap ,nsatim ,nsaman ,ntmpgr ,itim   ,
     &                   curtim ,istep  ,nstep  ,first  ,writim ,
     &                   wrirst, juer, fd_nefis_res, fd_nefis_rst,
     &                   sacpre ,salmap ,saltim ,csa    ,
     &                   csd    ,disgr  ,cdcdx  ,thasca ,mouqpu ,
     &                   thcsum ,timout ,sbdscr ,sbdpar ,rho    ,
     &                   tmpgr  ,ncelsa ,ker    ,dt     ,salini ,
     &                   buffer ,gridnm ,nefhis                 )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAWRES (SAlt Write RESults)
c
c Module description: Write salt results and restart information.
c
c                     This subroutine will call the result writing rou-
c                     tine and the restart file writing routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 27 cdcdx             P  -
c 24 csa               P  -
c 25 csd               P  -
c 18 dafdrs            P  -
c 20 dafdst            P  -
c 17 defdrs            P  -
c 19 defdst            P  -
c 26 disgr             P  -
c  1 dsopt             P  -
c 13 first             O  True in case of first call.
c 11 istep             P  -
c 10 itim              P  -
c 11 curtim            P  -
c 16 juer              P  -
c 36 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 29 mouqpu            P  -
c  3 nboun             I  Number of boundary nodes.
c  4 nbran             I  Number of branches.
c 35 ncelsa            P  -
c  5 ngrid             I  Number of grid points in network.
c  2 nmouth            P  -
c  8 nsaman            I  Number of main codes of salt results.
c  6 nsamap            I  Number of entries in salmap.
c  7 nsatim            I  Number of entries in saltim.
c 12 nstep             P  -
c  9 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 33 rho               P  -
c 21 sacpre            P  -
c 22 salmap            P  -
c 23 saltim            P  -
c 32 sbdscr            P  -
c 32 sbdpar            P  -
c 28 thasca            P  -
c 30 thcsum            P  -
c 31 timout            P  -
c 34 tmpgr             P  -
c 15 wrirst            I  True when restart info must be written.
c 14 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c sarsta  SAlt read or write of ReSTArt information
c sasar   SAlt SAlt Results
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
c $Log: sawres.pf,v $
c Revision 1.8  1997/06/18  07:52:11  kuipe_j
c Remove unreferenced vars
c
c Revision 1.7  1997/06/17  11:27:10  kuipe_j
c output in history format
c
c Revision 1.6  1996/12/02  15:31:44  kuipe_j
c Salt restart improved
c
c Revision 1.5  1996/02/09  15:13:33  kuipe_j
c a.o. Restart improvements
c
c Revision 1.4  1996/01/16  15:01:46  kuipe_j
c Restart improvements
c
c Revision 1.3  1995/05/30  09:56:19  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:24  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:06  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/02/10  09:15:25  kuipe_j
c Some minor changes for beta release
c
c Revision 1.2  1993/11/26  15:34:13  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nentri
      parameter (nentri=6)
      integer dsopt ,nmouth  ,nboun  ,nbran    ,ngrid     ,nsamap    ,
     &        nsatim,nsaman  ,ntmpgr ,juer     ,istep     ,nstep     ,
     &        ker   ,nefhis
      integer fd_nefis_res, fd_nefis_rst,
     &        salmap(nsamap)  ,saltim(nsatim)  ,itim  (2) ,
     &        sacpre(nsaman)  ,ncelsa(3)       
      integer salini(*)
      real    csa   (ngrid,2) ,csd   (ngrid,2) ,rho  (ngrid),
     &        cdcdx (ngrid,3) ,disgr (ngrid)   ,tmpgr(ngrid,ntmpgr)  ,
     &        thasca(3    )   ,mouqpu(3,0:2,*) ,curtim               ,
     &        thcsum(2,nbran) ,timout(2,*)     ,sbdscr(3,nboun)      ,
     &        sbdpar(5,nboun) ,buffer(nentri,ngrid) 
      double precision dt
      character*40     gridnm(*)

      logical first ,writim   ,wrirst
c
c     Declaration of local variables
c
      integer                 :: fd_nefis_dum = -1
      integer code(nentri)
      logical inires
c
      include '..\include\errcod.i'
c
      data (code(i),i=1,nentri) / 1,2,3,4,5,6/
c
      ker = ok
c
      if (first) call resadm (nentri ,code   ,sacpre )
c
      if (nefhis.ge.1) then
c     NEFIS output
      call sasar (fd_nefis_res, nsaman  ,nsamap ,nsatim   ,ngrid    ,
     &            itim   ,istep  ,nstep   ,first  ,writim   ,juer     ,
c                                <csa2>           <ncelm>   <ncelh>
     &            salmap ,saltim ,csa(1,2),disgr ,ncelsa(1),ncelsa(2),
     &            sacpre ,rho    ,tmpgr(1,1)      ,ker    )
c
      endif 
c
      if (nefhis.ne.1.and.dt.gt.0.0D0) then
c     HIS output
c     dt = negative for estuary morfology step, so no output
c

      call sawrhi(nsaman ,nsamap ,nsatim   ,ngrid  ,itim   ,istep  ,
c                                        <csa2>          
     &            nstep  ,salmap ,saltim ,csa(1,2) ,disgr  ,
     &            sacpre ,rho    ,dt       ,salini ,buffer ,gridnm )
      endif
      if (ker .eq. fatal) goto 1000
c
c     Set first to false
c
      first = .false.
c
      if (wrirst) then
         call sarsta (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid   ,itim    ,
     &                curtim ,juer   ,first  ,.false.,
     &                fd_nefis_rst, fd_nefis_dum,
c                       <csa2>         <csd2>
     &                csa(1,2)       ,csd(1,2)       ,
c                     <cdcdx0>        <cdcdx1>        <cdcdx2>
     &                cdcdx(1,1)     ,cdcdx(1,2)     ,cdcdx(1,3)       ,
     &                thasca ,mouqpu ,thcsum ,
c                                     <ncelst>
     &                timout ,sbdscr ,sbdpar ,ncelsa(3) ,inires  ,ker  )
      endif
c
 1000 continue
      end
