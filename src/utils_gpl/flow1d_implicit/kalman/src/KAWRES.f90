subroutine KAWRES(itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
&ngrid  ,buf    ,wrirst ,lfilt  ,&
&fd_nefis_res, fd_nefis_rst,&
&nkphmp ,nkphtm ,nphymn ,kphmap ,kphtim ,phycpr ,&
&nkppmp ,nkpptm ,nppamn ,kppmap ,kpptim ,ppacpr ,&
&nkfhmp ,nkfhtm ,nfhymn ,kfhmap ,kfhtim ,fhycpr ,&
&nkfpmp ,nkfptm ,nfpamn ,kfpmap ,kfptim ,fpacpr ,&
&nkfrmp ,nkfrtm ,nfremn ,kfrmap ,kfrtim ,frecpr ,&
&nkfgmp ,nfgamn ,kfgmap ,fgacpr ,nkfmmp ,nfcpmn ,&
&kfmmap ,fcpcpr ,cpredn ,np     ,p1     ,p2     ,&
&hp     ,qp     ,nnf    ,pfa    ,nnmu   ,pmua   ,&
&pw     ,nkapar ,nsamp  ,res    ,scares ,rescov ,&
&kgain  ,nclphy ,nclppa ,nclfhy ,nclfpa ,nclfre ,&
&nclfga ,nclfcp ,nclrst ,smploc ,gridnm ,corrnm ,&
&kalini ,nefhis ,dt     ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAWRES (KAlman Write RESults)
!
! Module description: In subroutine KAWRES at the end of each time step
!                     Kalman results will be written to the result file
!                     if requested.
!
!                     For each Nefis group a routine will be called to
!                     write user selected results. Then at certain time
!                     levels, restart information will be written to a
!                     restart file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 buf               P  -
! 53 cpredn            P  -
! 11 dafdrs            P  -
! 13 dafdst            P  -
! 12 defdrs            P  -
! 14 defdst            P  -
! 52 fcpcpr            P  -
! 48 fgacpr            P  -
! 32 fhycpr            P  -
!  4 first             O  True in case of first call.
! 38 fpacpr            P  -
! 44 frecpr            P  -
! 57 hp                P  -
!  2 istep             P  -
!  1 itim              P  -
!  6 juer              P  -
! 78 ker               P  -
! 47 kfgmap            P  -
! 30 kfhmap            P  -
! 31 kfhtim            P  -
! 51 kfmmap            P  -
! 36 kfpmap            P  -
! 37 kfptim            P  -
! 42 kfrmap            P  -
! 43 kfrtim            P  -
! 69 kgain             P  -
! 18 kphmap            P  -
! 19 kphtim            P  -
! 24 kppmap            P  -
! 25 kpptim            P  -
! 10 lfilt             P  -
! 76 nclfcp            P  -
! 75 nclfga            P  -
! 72 nclfhy            P  -
! 73 nclfpa            P  -
! 74 nclfre            P  -
! 70 nclphy            P  -
! 71 nclppa            P  -
! 77 nclrst            P  -
! 50 nfcpmn            I  Number of main codes of parameter covariance
! 46 nfgamn            I  Number of main codes of the Kalman gain.
! 29 nfhymn            I  Number of main codes of hydrodynamic filter
! 35 nfpamn            I  Number of main codes of parameter filter
! 41 nfremn            I  Number of main codes of residuals.
!  7 ngrid             I  Number of grid points in network.
! 64 nkapar            P  -
! 45 nkfgmp            I  Number of entries in kfgmap.
! 27 nkfhmp            I  Number of entries in kfhmap.
! 28 nkfhtm            I  Number of entries in kfhtim.
! 49 nkfmmp            I  Number of entries in kfmmap.
! 33 nkfpmp            I  Number of entries in kfpmap.
! 34 nkfptm            I  Number of entries in kfptim.
! 39 nkfrmp            I  Number of entries in kfrmap.
! 40 nkfrtm            I  Number of entries in kfrtim.
! 15 nkphmp            I  Number of entries in kphmap.
! 16 nkphtm            I  Number of entries in kphtim.
! 21 nkppmp            I  Number of entries in kppmap.
! 22 nkpptm            I  Number of entries in kpptim.
! 59 nnf               I  Number of uncertain bed friction parameters.
! 61 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 54 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 17 nphymn            I  Number of main codes of hydrodynamic prediction
! 23 nppamn            I  Number of main codes of parameter prediction
! 65 nsamp             I  Number of hydrodynamic samples (measurements)
!  3 nstep             P  -
! 55 p1                P  -
! 56 p2                P  -
! 60 pfa               P  -
! 20 phycpr            P  -
! 62 pmua              P  -
! 26 ppacpr            P  -
! 63 pw                P  -
! 58 qp                P  -
! 66 res               P  -
! 68 rescov            P  -
! 67 scares            P  -
!  9 wrirst            I  True when restart info must be written.
!  5 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kafcpr  KAlman Filter Covariance Parameter Results
! kafgar  KAlman Filter kalman GAin Results
! kafhyr  KAlman Filtered HYdraulic Results
! kafpar  KAlman Filtered PArameter Results
! kafrer  KAlman Filter REsidual Results
! kaphyr  KAlman Predicted HYdraulic Results
! kappar  KAlman Predicted PArameter Results
! karsta  KAlman read or write ReSTArt information
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kawres.pf,v $
! Revision 1.3  1997/06/17  11:26:51  kuipe_j
! output in history format
!
! Revision 1.2  1996/04/12  13:05:37  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:11  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer      ngrid  ,juer   ,istep  ,nstep  ,&
   &ker    ,np     ,nkapar ,nnf    ,nnmu   ,&
   &nsamp  ,cpredn ,nefhis
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

!
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
!
!     Declaration of local variables
!
   logical inires ,llog
!
   if      (nefhis.ne.1.and.dt.gt.0.0D0) then
!     HIS uitvoer
!     dt = negative for estuary morfology step, so no output
!
      call KAPHYH (nphymn ,nkphmp ,nkphtm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,&
      &kphmap ,kphtim ,cpredn         ,np     ,p2     ,&
      &phycpr ,buf    ,gridnm         ,kalini ,dt     )
!
      call KAPPAH (nppamn ,nkppmp ,nkpptm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,&
      &kppmap ,kpptim ,cpredn         ,np     ,p2     ,&
      &nkapar ,ppacpr ,buf    ,corrnm ,kalini ,dt     )
!
      call KAFHYH (nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,lfilt  ,&
      &kfhmap ,kfhtim ,hp(1,3),qp(1,3),np     ,&
      &p1     ,fhycpr ,buf    ,gridnm ,kalini ,dt     )
!
      call KAFPAH (nfpamn ,nkfpmp ,nkfptm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,lfilt  ,&
      &kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,&
      &pmua   ,pw     ,np     ,p1     ,nkapar ,&
      &fpacpr ,buf    ,corrnm ,kalini ,dt     )
!
      call KAFREH (nfremn ,nkfrmp ,nkfrtm ,nsamp  ,&
      &itim   ,istep  ,nstep  ,first  ,lfilt  ,&
      &kfrmap ,kfrtim ,res    ,scares ,rescov ,&
      &frecpr ,buf    ,smploc ,gridnm ,ngrid  ,kalini ,&
      &dt     )
!
   endif
!
   if (nefhis.ge.1) then
!     NEFIS uitvoer
      call KAPHYR (fd_nefis_res, nphymn ,nkphmp ,nkphtm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
      &kphmap ,kphtim ,cpredn         ,np     ,p2     ,&
      &nclphy(1)      ,nclphy(2)      ,phycpr ,buf    ,&
      &ker    )
!
      call KAPPAR (fd_nefis_res, nppamn ,nkppmp ,nkpptm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
      &kppmap ,kpptim ,cpredn         ,np     ,p2     ,&
      &nkapar ,nclppa(1)      ,nclppa(2)      ,ppacpr ,&
      &buf    ,ker    )
!
      call KAFHYR (fd_nefis_res, nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
      &juer   ,kfhmap ,kfhtim ,hp(1,3),qp(1,3),np     ,&
      &p1     ,nclfhy(1)      ,nclfhy(2)      ,fhycpr ,&
      &buf    ,ker    )
!
      call KAFPAR (fd_nefis_res, nfpamn ,nkfpmp ,nkfptm ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
      &juer   ,kfpmap ,kfptim ,nnf    ,pfa    ,nnmu   ,&
      &pmua   ,pw     ,np     ,p1     ,nkapar ,&
      &nclfpa(1)      ,nclfpa(2)      ,fpacpr ,buf    ,&
      &ker    )
!
      call KAFRER (fd_nefis_res, nfremn ,nkfrmp ,nkfrtm ,nsamp  ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
      &juer   ,kfrmap ,kfrtim ,res    ,scares ,rescov ,&
      &nclfre(1)      ,nclfre(2)      ,frecpr ,buf    ,&
      &ker    )
!
      call KAFGAR (fd_nefis_res, nfgamn ,nkfgmp ,itim   ,&
      &istep  ,nstep  ,first  ,writim ,lfilt  ,juer   ,&
      &kfgmap ,nclfga ,fgacpr ,np     ,nsamp  ,kgain  ,&
      &ker    )
!
!     The predicted covariances stored in P2 are no longer needed.
!     So array P2 will be used as scratch space for output of
!     filtered covariances of parameters.
!
      call KAFCPR (fd_nefis_res, nfcpmn ,nkfmmp ,np     ,nkapar ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
      &juer   ,kfmmap ,p1     ,p2     ,nclfcp ,fcpcpr ,&
      &ker    )
!
   endif
!
   if ( wrirst ) then
      llog = .false.
      call KARSTA (np     ,nnf    ,nnmu   ,itim   ,juer   ,&
      &llog , .false., fd_nefis_rst, fd_nefis_dum,&
      &p1     ,pfa    ,pmua   ,pw     ,&
      &nclrst ,inires ,ker    )
   endif
!
!     Set first to false
!
   first = .false.
!
end
