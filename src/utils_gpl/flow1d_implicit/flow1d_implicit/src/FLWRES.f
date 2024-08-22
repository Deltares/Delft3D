      subroutine FLWRES (dt     ,psi    ,theta  ,first  ,itim   ,curtim,
     &                   istep  ,nstep  ,wqagst ,writim ,dlwqts ,nstru ,
     &                   ncontr ,ngrid  ,nhyman ,nhymap ,nhytim ,nlaman,
     &                   nlatim ,nqlat  ,nstman ,nsttim ,ntmpgr ,Iwqin ,
     &                   wrirst ,lagstm ,nlags  ,g      ,afwfqs ,contrl,
     &                   conhis ,strhis ,cp     ,rp     ,
     &                   fd_nefis_res, fd_nefis_rst, fd_nefis_waq,
     &                   hp     ,hycpre ,hyrmap ,hyrtim ,lacpre ,strtim,
     &                   lattim ,qaggr  ,qlaggr ,qlat   ,qp     ,stcpre,
     &                   tmpgr  ,waoft  ,ncelfl ,nclstr ,ncllat ,arexop,
     &                   arexcn ,lfilt  ,juer   ,ker    ,flwini ,gridnm,
     &                   strunm ,qlatnm ,buffer ,nefhis ,qltpar ,
     &                   grhis  ,lgrwt  ,buflag)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLWRES (FLow Write RESults)
c
c Module description: In subroutine FLWRES at the end of each time step
c                     a check will be made if flow results have to be
c                     written to the result file.
c
c                     First routines FLHY* will be called to write user
c                     selected hydrodynamic results. When water quality
c                     has been activated routine FLAGGR will be called
c                     to aggregate hydrodynamic information. This infor-
c                     mation will be written by routine FLHYWQ. Finally,
c                     at certain time levels, restart information will
c                     be written to a restart file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 26 afwfqs            P  -
c 55 arexcn            P  -
c 54 arexop            P  -
c 28 conhis            P  -
c 27 contrl            P  -
c 30 cp                P  -
c  6 curtim            P  -
c 31 dafdrs            P  -
c 32 dafdst            P  -
c 33 dafdwq            P  -
c 34 defdrs            P  -
c 35 defdst            P  -
c 36 defdwq            P  -
c 11 dlwqts            P  -
c  1 dt                P  -
c  4 first             IO True in case of first call.
c 37 hp                P  -
c 38 hycpre            P  -
c 39 hyrmap            P  -
c 40 hyrtim            P  -
c  7 istep             I  Current time step number (t(n+1)).
c  5 itim              P  -
c 57 juer              P  -
c 58 ker               P  -
c 41 lacpre            P  -
c 43 lattim            P  -
c 56 lfilt             I  = True if a filter step must be performed.
c 24 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c 51 ncelfl            P  -
c 53 ncllat            P  -
c 52 nclstr            P  -
c 13 ncontr            P  -
c 14 ngrid             I  Number of grid points in network.
c 15 nhyman            I  Number of main codes of hydrodynamic results.
c 16 nhymap            I  Number of entries in hyrmap.
c 17 nhytim            I  Number of entries in hyrtim.
c 18 nlaman            I  Number of main codes of lateral discharges
c                         results.
c 19 nlatim            I  Number of entries in lattim.
c 20 nqlat             P  -
c  8 nstep             P  -
c 21 nstman            I  Number of main codes of structures results.
c 12 nstru             P  -
c 22 nsttim            I  Number of entries in strtim.
c 23 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c  2 psi               P  -
c 44 qaggr             P  -
c 45 qlaggr            P  -
c 46 qlat              P  -
c    qltpar            P  -
c 47 qp                P  -
c 48 stcpre            P  -
c 29 strhis            P  -
c 42 strtim            P  -
c  3 theta             P  -
c 49 tmpgr             P  -
c 50 waoft             P  -
c  9 wqagst            I  Time step of the water quality process run.
c 25 wrirst            I  True when restart info must be written.
c 10 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flaggr  FLow AGGRegate
c flhy1   FLow HYdrodynamic results 1
c flhy2   FLow HYdrodynamic results 2
c flhy3   FLow HYdrodynamic results 3
c flhywq  FLow HYdrodynamic results to Water Quality
c flrsta  FLow read or write of ReSTArt information
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flwres.pf,v $
c Revision 1.17  1999/03/15  14:19:34  kuipe_j
c improve writing Aggr-file
c
c Revision 1.16  1998/06/08  12:35:56  kuipe_j
c log added
c
c Revision 1.15  1997/11/04  14:17:32  kuipe_j
c Retention basin
c
c Revision 1.14  1997/06/17  11:26:36  kuipe_j
c output in history format
c
c Revision 1.12  1997/01/23  08:29:20  kuipe_j
c Make flow module robust
c
c Revision 1.11  1996/09/03  14:52:07  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.10  1996/04/12  13:04:27  kuipe_j
c headers, minor changes
c
c Revision 1.9  1996/04/11  08:23:58  kuipe_j
c Kalman module added
c
c Revision 1.8  1996/01/17  14:38:58  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:28  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/09/22  10:02:35  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:08  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:01  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:39  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:41  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:19  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  08:38:00  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:43:57  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters
c
      integer    nenhyc, nenstr, nenlat
      parameter (nenhyc=34, nenstr=4, nenlat=2)
      integer nhyman ,nhymap ,nhytim ,ngrid  ,juer   ,istep  ,nstep  ,
     &        ntmpgr ,ker    ,nstman ,nsttim ,nstru  ,ncontr ,nlaman ,
     &        nlatim ,nqlat  ,wqagst ,dlwqts ,nclstr ,ncllat ,nefhis ,
     &        lagstm ,nlags  ,Iwqin
      integer ncelfl(3)      ,itim  (2)      ,
     &        fd_nefis_res      ,
     &        fd_nefis_rst      ,
     &        fd_nefis_waq      ,
     &        hyrmap(nhymap) ,hyrtim(nhytim) ,hycpre(nhyman) ,
     &        strtim(nsttim) ,stcpre(nstman) ,
     &        lattim(nlatim) ,lacpre(nlaman) ,
     &        arexop(2)      ,arexcn(ngrid,2)
      integer flwini(*)
      real    theta  ,psi    ,curtim          ,g
      real    cp    (ngrid,4),
     &        waoft(ngrid,*) ,afwfqs(ngrid,8) ,rp    (ngrid,4),
     &        tmpgr (ngrid,ntmpgr)            ,
     &        qlat (*)       ,qaggr (ngrid,3) ,qlaggr(*)      ,
     &        conhis(5,*)    ,strhis(dmstrh,*),contrl(17,*)   ,
     &        buffer(nenhyc-1,ngrid)   ,qltpar(9,*)
      real    buflag(lagstm,nlags), grhis(*)
      double  precision  dt, hp(ngrid,3), qp(ngrid,3)
      logical first  ,writim ,wrirst,  lfilt, lgrwt
      character(len=40) gridnm(*), strunm(*), qlatnm(*)
c
c     Declaration of local variables
c
      integer fd_nefis_dum    ,i ,   j
      integer codhyc(nenhyc) , codstr(nenstr), codlat(nenlat)
      logical lwqin  ,inires , aggr, llog
c
      data (codhyc(i),i=1,nenhyc) /1 ,2 ,2 ,2 ,2 ,3 ,4 ,4 ,4 ,4 ,
     +                             5 ,5 ,5 ,5 ,6 ,6 ,6 ,6 ,7 ,8 ,
     +                             9 ,9 ,9 ,9 ,10,10,10,10,11,11,
     +                             11,11,12,13/
      data (codstr(i),i=1,nenstr) /1,1,1,2                            /
      data (codlat(i),i=1,nenlat) /1,2                                /
c
      if ( lfilt ) then
         j = 2
      else
         j = 3
      endif
c
c     Generate pre codes
c
      if (first) then
         call resadm (nenhyc, codhyc, hycpre)
         call resadm (nenstr, codstr, stcpre)
         call resadm (nenlat, codlat, lacpre)
      endif
c
      if (nefhis.ge.1) then
c     NEFIS output
      call FLHY1 (fd_nefis_res ,nhyman ,nhymap ,nhytim ,ngrid  ,
     &            itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &            hyrmap ,hyrtim ,hp(1,j),qp(1,j),afwfqs(1,7)    ,
     &            waoft(1,4)     ,waoft(1,3)     ,afwfqs(1,1)    ,
     &            cp(1,1),cp(1,2),waoft(1,1)     ,afwfqs(1,3)    ,
     &            ncelfl(1)      ,ncelfl(2)      ,hycpre         ,
     &            tmpgr(1,1)     ,ker            )
c
      call FLHY2 (fd_nefis_res ,nstman ,nsttim ,ngrid  ,itim   ,
     &            istep  ,nstep  ,first  ,writim ,juer   ,strtim ,
     &            nstru  ,strhis ,nclstr ,stcpre ,
     &            tmpgr(1,1)     ,ker    )
c
      call FLHY3 (fd_nefis_res ,nlaman ,nlatim ,ngrid  ,itim   ,
     &            istep  ,nstep  ,first  ,writim ,juer   ,lattim ,
     &            nqlat  ,qlat   ,ncllat ,lacpre ,tmpgr(1,1)     ,
     &            ker    ,qltpar ,strhis ,nstru  )
c
      endif
c
      if (nefhis.ne.1.and.dt.gt.0.0D0) then
c       HIS output
c       dt = negative for estuary morfology step, so no output
c
         call FLWRHI(hp(1,j), qp(1,j), ngrid  , itim   , flwini,
     &               strhis , nstru  , istep  , nstep  , dt    , qlat  ,
     &               nqlat  , hyrtim , nhytim , hycpre , afwfqs(1,7)   ,
     &               waoft(1,4)      , waoft(1,3)      , afwfqs(1,1)   ,
     &               cp(1,1), cp(1,2), waoft(1,1)      , afwfqs(1,3)   ,
     &               waoft(1,2)      , rp(1,1), rp(1,2), g     , nsttim,
     &               strtim , stcpre , nlatim , lattim , lacpre, hyrmap,
     &               nhymap , gridnm , strunm , qlatnm , buffer, qltpar)
      endif
c
 
      lwqin = Iwqin.gt.0
      if ( lwqin ) then
          call FLAGGR(istep  ,wqagst ,theta  ,ngrid  ,nqlat       ,
     &                qp(1,1),qp(1,3),afwfqs(1,5)    ,afwfqs(1,7) ,
     &                qlat   ,qaggr  ,qlaggr )
c
        aggr = mod(istep,wqagst) .eq. 0
        if ( first .or. aggr .or. istep.ge.nstep ) then
           if (Iwqin .eq. 1) then
c             Standard call           
              call FLHYWQ  (fd_nefis_waq ,ngrid  ,nqlat  ,dlwqts ,
     &                      abs(dt),itim   ,first  ,aggr   ,wqagst ,
c                                                   Af(n+THETA)
     &                      writim ,istep  ,nstep  ,waoft(1,3)     ,
c                                           At(n+1)
     &                      afwfqs(1,1)    ,waoft(1,4)     ,cp(1,1),
     &                      psi    ,qaggr  ,
c                                   Wt(n+1)
     &                      qlaggr ,waoft(1,2)     ,afwfqs(1,3)    ,
     &                      juer   ,ker    )
           endif 
        endif  
      endif
      if ( wrirst ) then
         llog = .false.
         call FLRSTA (lwqin  ,nqlat  ,nstru  ,ncontr ,ngrid  ,itim   ,
     &                curtim ,juer   ,llog   ,.false., fd_nefis_rst,
     &                fd_nefis_dum ,hp(1,3),qp(1,3),contrl ,conhis ,
     &                strhis ,qaggr  ,qlaggr ,ncelfl(3)      ,inires ,
     &                arexop ,arexcn ,lagstm ,nlags  ,
     &                lgrwt  ,grhis  ,buflag ,ker    )
      endif
c
c     Set first to false
c
      first = .false.
c
      end
