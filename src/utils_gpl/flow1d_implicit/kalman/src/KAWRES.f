      subroutine KAWRES(itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &                  ngrid  ,buf    ,wrirst ,lfilt  ,
     &                  fd_nefis_res, fd_nefis_rst,
     &                  nkphmp ,nkphtm ,nphymn ,kphmap ,kphtim ,phycpr ,
     &                  nkppmp ,nkpptm ,nppamn ,kppmap ,kpptim ,ppacpr ,
     &                  nkfhmp ,nkfhtm ,nfhymn ,kfhmap ,kfhtim ,fhycpr ,
     &                  nkfpmp ,nkfptm ,nfpamn ,kfpmap ,kfptim ,fpacpr ,
     &                  nkfrmp ,nkfrtm ,nfremn ,kfrmap ,kfrtim ,frecpr ,
     &                  nkfgmp ,nfgamn ,kfgmap ,fgacpr ,nkfmmp ,nfcpmn ,
     &                  kfmmap ,fcpcpr ,cpredn ,np     ,p1     ,p2     ,
     &                  hp     ,qp     ,nnf    ,pfa    ,nnmu   ,pmua   ,
     &                  pw     ,nkapar ,nsamp  ,res    ,scares ,rescov ,
     &                  kgain  ,nclphy ,nclppa ,nclfhy ,nclfpa ,nclfre ,
     &                  nclfga ,nclfcp ,nclrst ,smploc ,gridnm ,corrnm ,
     &                  kalini ,nefhis ,dt     ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAWRES (KAlman Write RESults)
c
c Module description: In subroutine KAWRES at the end of each time step
c                     Kalman results will be written to the result file
c                     if requested.
c
c                     For each Nefis group a routine will be called to
c                     write user selected results. Then at certain time
c                     levels, restart information will be written to a
c                     restart file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 buf               P  -
c 53 cpredn            P  -
c 11 dafdrs            P  -
c 13 dafdst            P  -
c 12 defdrs            P  -
c 14 defdst            P  -
c 52 fcpcpr            P  -
c 48 fgacpr            P  -
c 32 fhycpr            P  -
c  4 first             O  True in case of first call.
c 38 fpacpr            P  -
c 44 frecpr            P  -
c 57 hp                P  -
c  2 istep             P  -
c  1 itim              P  -
c  6 juer              P  -
c 78 ker               P  -
c 47 kfgmap            P  -
c 30 kfhmap            P  -
c 31 kfhtim            P  -
c 51 kfmmap            P  -
c 36 kfpmap            P  -
c 37 kfptim            P  -
c 42 kfrmap            P  -
c 43 kfrtim            P  -
c 69 kgain             P  -
c 18 kphmap            P  -
c 19 kphtim            P  -
c 24 kppmap            P  -
c 25 kpptim            P  -
c 10 lfilt             P  -
c 76 nclfcp            P  -
c 75 nclfga            P  -
c 72 nclfhy            P  -
c 73 nclfpa            P  -
c 74 nclfre            P  -
c 70 nclphy            P  -
c 71 nclppa            P  -
c 77 nclrst            P  -
c 50 nfcpmn            I  Number of main codes of parameter covariance
c 46 nfgamn            I  Number of main codes of the Kalman gain.
c 29 nfhymn            I  Number of main codes of hydrodynamic filter
c 35 nfpamn            I  Number of main codes of parameter filter
c 41 nfremn            I  Number of main codes of residuals.
c  7 ngrid             I  Number of grid points in network.
c 64 nkapar            P  -
c 45 nkfgmp            I  Number of entries in kfgmap.
c 27 nkfhmp            I  Number of entries in kfhmap.
c 28 nkfhtm            I  Number of entries in kfhtim.
c 49 nkfmmp            I  Number of entries in kfmmap.
c 33 nkfpmp            I  Number of entries in kfpmap.
c 34 nkfptm            I  Number of entries in kfptim.
c 39 nkfrmp            I  Number of entries in kfrmap.
c 40 nkfrtm            I  Number of entries in kfrtim.
c 15 nkphmp            I  Number of entries in kphmap.
c 16 nkphtm            I  Number of entries in kphtim.
c 21 nkppmp            I  Number of entries in kppmap.
c 22 nkpptm            I  Number of entries in kpptim.
c 59 nnf               I  Number of uncertain bed friction parameters.
c 61 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 54 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 17 nphymn            I  Number of main codes of hydrodynamic prediction
c 23 nppamn            I  Number of main codes of parameter prediction
c 65 nsamp             I  Number of hydrodynamic samples (measurements)
c  3 nstep             P  -
c 55 p1                P  -
c 56 p2                P  -
c 60 pfa               P  -
c 20 phycpr            P  -
c 62 pmua              P  -
c 26 ppacpr            P  -
c 63 pw                P  -
c 58 qp                P  -
c 66 res               P  -
c 68 rescov            P  -
c 67 scares            P  -
c  9 wrirst            I  True when restart info must be written.
c  5 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kafcpr  KAlman Filter Covariance Parameter Results
c kafgar  KAlman Filter kalman GAin Results
c kafhyr  KAlman Filtered HYdraulic Results
c kafpar  KAlman Filtered PArameter Results
c kafrer  KAlman Filter REsidual Results
c kaphyr  KAlman Predicted HYdraulic Results
c kappar  KAlman Predicted PArameter Results
c karsta  KAlman read or write ReSTArt information
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kawres.pf,v $
c Revision 1.3  1997/06/17  11:26:51  kuipe_j
c output in history format
c
c Revision 1.2  1996/04/12  13:05:37  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:11  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer      ngrid  ,juer   ,istep  ,nstep  ,
     &             ker    ,np     ,nkapar ,nnf    ,nnmu   ,
     &             nsamp  ,cpredn ,nefhis
      integer      fd_nefis_res, fd_nefis_rst, itim(2)
      integer      kalini(10)
      real         buf(*)
      real         p1(np,np)      ,p2(np,np)
      real         pfa(nnf)       ,pmua(nnmu)    ,pw
      real         res(nsamp)     ,scares(nsamp)
      real         rescov(nsamp,nsamp)
      real         kgain(np*nsamp)
      logical      first  ,writim ,wrirst      ,lfilt
      double precision dt ,hp(ngrid,3), qp(ngrid,3)

c
      integer      nkphmp         ,nkphtm         ,nphymn
      integer      kphmap(nkphmp) ,kphtim(nkphtm) ,phycpr(nphymn)
      integer      nclphy(2)
      integer      nkppmp         ,nkpptm         ,nppamn
      integer      kppmap(nkppmp) ,kpptim(nkpptm) ,ppacpr(nppamn)
      integer      nclppa(2)
      integer      nkfhmp         ,nkfhtm         ,nfhymn
      integer      kfhmap(nkfhmp) ,kfhtim(nkfhtm) ,fhycpr(nfhymn)
      integer      nclfhy(2)
      integer      nkfpmp         ,nkfptm         ,nfpamn
      integer      kfpmap(nkfpmp) ,kfptim(nkfptm) ,fpacpr(nfpamn)
      integer      nclfpa(2)
      integer      nkfrmp         ,nkfrtm         ,nfremn
      integer      kfrmap(nkfrmp) ,kfrtim(nkfrtm) ,frecpr(nfremn)
      integer      nclfre(2)
      integer      nkfgmp         ,nfgamn
      integer      kfgmap(nkfgmp) ,fgacpr(nfgamn)
      integer      nclfga
      integer      nkfmmp         ,nfcpmn
      integer      kfmmap(nkfmmp) ,fcpcpr(nfcpmn)
      integer      nclfcp
      integer      nclrst
      integer                  :: fd_nefis_dum = -1
      integer      smploc(nsamp)                   
      character*40 corrnm(*)  
      character*40 gridnm(ngrid)
c
c     Declaration of local variables
c
      logical inires ,llog
c
      if      (nefhis.ne.1.and.dt.gt.0.0D0) then
c     HIS uitvoer
c     dt = negative for estuary morfology step, so no output
c
      call KAPHYH (nphymn ,nkphmp ,nkphtm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,
     &             kphmap ,kphtim ,cpredn         ,np     ,p2     ,
     &             phycpr ,buf    ,gridnm         ,kalini ,dt     )
c
      call KAPPAH (nppamn ,nkppmp ,nkpptm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,
     &             kppmap ,kpptim ,cpredn         ,np     ,p2     ,
     &             nkapar ,ppacpr ,buf    ,corrnm ,kalini ,dt     )
c
      call KAFHYH (nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,lfilt  ,
     &             kfhmap ,kfhtim ,hp(1,3),qp(1,3),np     ,
     &             p1     ,fhycpr ,buf    ,gridnm ,kalini ,dt     )
c
      call KAFPAH (nfpamn ,nkfpmp ,nkfptm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,lfilt  ,
     &             kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,
     &             pmua   ,pw     ,np     ,p1     ,nkapar ,
     &             fpacpr ,buf    ,corrnm ,kalini ,dt     )
c
      call KAFREH (nfremn ,nkfrmp ,nkfrtm ,nsamp  ,
     &             itim   ,istep  ,nstep  ,first  ,lfilt  ,
     &             kfrmap ,kfrtim ,res    ,scares ,rescov ,
     &             frecpr ,buf    ,smploc ,gridnm ,ngrid  ,kalini ,
     &             dt     )
c
      endif
c
      if (nefhis.ge.1) then
c     NEFIS uitvoer
      call KAPHYR (fd_nefis_res, nphymn ,nkphmp ,nkphtm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &             kphmap ,kphtim ,cpredn         ,np     ,p2     ,
     &             nclphy(1)      ,nclphy(2)      ,phycpr ,buf    ,
     &             ker    )
c
      call KAPPAR (fd_nefis_res, nppamn ,nkppmp ,nkpptm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,writim ,juer   ,
     &             kppmap ,kpptim ,cpredn         ,np     ,p2     ,
     &             nkapar ,nclppa(1)      ,nclppa(2)      ,ppacpr ,
     &             buf    ,ker    )
c
      call KAFHYR (fd_nefis_res, nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &             juer   ,kfhmap ,kfhtim ,hp(1,3),qp(1,3),np     ,
     &             p1     ,nclfhy(1)      ,nclfhy(2)      ,fhycpr ,
     &             buf    ,ker    )
c
      call KAFPAR (fd_nefis_res, nfpamn ,nkfpmp ,nkfptm ,ngrid  ,
     &             itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &             juer   ,kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,
     &             pmua   ,pw     ,np     ,p1     ,nkapar ,
     &             nclfpa(1)      ,nclfpa(2)      ,fpacpr ,buf    ,
     &             ker    )
c
      call KAFRER (fd_nefis_res, nfremn ,nkfrmp ,nkfrtm ,nsamp  ,
     &             itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &             juer   ,kfrmap ,kfrtim ,res    ,scares ,rescov ,
     &             nclfre(1)      ,nclfre(2)      ,frecpr ,buf    ,
     &             ker    )
c
      call KAFGAR (fd_nefis_res, nfgamn ,nkfgmp ,itim   ,
     &             istep  ,nstep  ,first  ,writim ,lfilt  ,juer   ,
     &             kfgmap ,nclfga ,fgacpr ,np     ,nsamp  ,kgain  ,
     &             ker    )
c
c     The predicted covariances stored in P2 are no longer needed.
c     So array P2 will be used as scratch space for output of
c     filtered covariances of parameters.
c
      call KAFCPR (fd_nefis_res, nfcpmn ,nkfmmp ,np     ,nkapar ,
     &             itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,
     &             juer   ,kfmmap ,p1     ,p2     ,nclfcp ,fcpcpr ,
     &             ker    )
c
      endif
c
      if ( wrirst ) then
         llog = .false.
         call KARSTA (np     ,nnf    ,nnmu   ,itim   ,juer   ,
     &                llog , .false., fd_nefis_rst, fd_nefis_dum,
     &                p1     ,pfa    ,pmua   ,pw     ,
     &                nclrst ,inires ,ker    )
      endif
c
c     Set first to false
c
      first = .false.
c
      end
