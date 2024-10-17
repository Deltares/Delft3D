subroutine FLWRES (dt     ,psi    ,theta  ,first  ,itim   ,curtim,&
&istep  ,nstep  ,wqagst ,writim ,dlwqts ,nstru ,&
&ncontr ,ngrid  ,nhyman ,nhymap ,nhytim ,nlaman,&
&nlatim ,nqlat  ,nstman ,nsttim ,ntmpgr ,Iwqin ,&
&wrirst ,lagstm ,nlags  ,g      ,afwfqs ,contrl,&
&conhis ,strhis ,cp     ,rp     ,&
&fd_nefis_res, fd_nefis_rst, fd_nefis_waq,&
&hp     ,hycpre ,hyrmap ,hyrtim ,lacpre ,strtim,&
&lattim ,qaggr  ,qlaggr ,qlat   ,qp     ,stcpre,&
&tmpgr  ,waoft  ,ncelfl ,nclstr ,ncllat ,arexop,&
&arexcn ,lfilt  ,juer   ,ker    ,flwini ,gridnm,&
&strunm ,qlatnm ,buffer ,nefhis ,qltpar ,&
&grhis  ,lgrwt  ,buflag)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLWRES (FLow Write RESults)
!
! Module description: In subroutine FLWRES at the end of each time step
!                     a check will be made if flow results have to be
!                     written to the result file.
!
!                     First routines FLHY* will be called to write user
!                     selected hydrodynamic results. When water quality
!                     has been activated routine FLAGGR will be called
!                     to aggregate hydrodynamic information. This infor-
!                     mation will be written by routine FLHYWQ. Finally,
!                     at certain time levels, restart information will
!                     be written to a restart file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 26 afwfqs            P  -
! 55 arexcn            P  -
! 54 arexop            P  -
! 28 conhis            P  -
! 27 contrl            P  -
! 30 cp                P  -
!  6 curtim            P  -
! 31 dafdrs            P  -
! 32 dafdst            P  -
! 33 dafdwq            P  -
! 34 defdrs            P  -
! 35 defdst            P  -
! 36 defdwq            P  -
! 11 dlwqts            P  -
!  1 dt                P  -
!  4 first             IO True in case of first call.
! 37 hp                P  -
! 38 hycpre            P  -
! 39 hyrmap            P  -
! 40 hyrtim            P  -
!  7 istep             I  Current time step number (t(n+1)).
!  5 itim              P  -
! 57 juer              P  -
! 58 ker               P  -
! 41 lacpre            P  -
! 43 lattim            P  -
! 56 lfilt             I  = True if a filter step must be performed.
! 24 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
! 51 ncelfl            P  -
! 53 ncllat            P  -
! 52 nclstr            P  -
! 13 ncontr            P  -
! 14 ngrid             I  Number of grid points in network.
! 15 nhyman            I  Number of main codes of hydrodynamic results.
! 16 nhymap            I  Number of entries in hyrmap.
! 17 nhytim            I  Number of entries in hyrtim.
! 18 nlaman            I  Number of main codes of lateral discharges
!                         results.
! 19 nlatim            I  Number of entries in lattim.
! 20 nqlat             P  -
!  8 nstep             P  -
! 21 nstman            I  Number of main codes of structures results.
! 12 nstru             P  -
! 22 nsttim            I  Number of entries in strtim.
! 23 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
!  2 psi               P  -
! 44 qaggr             P  -
! 45 qlaggr            P  -
! 46 qlat              P  -
!    qltpar            P  -
! 47 qp                P  -
! 48 stcpre            P  -
! 29 strhis            P  -
! 42 strtim            P  -
!  3 theta             P  -
! 49 tmpgr             P  -
! 50 waoft             P  -
!  9 wqagst            I  Time step of the water quality process run.
! 25 wrirst            I  True when restart info must be written.
! 10 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flaggr  FLow AGGRegate
! flhy1   FLow HYdrodynamic results 1
! flhy2   FLow HYdrodynamic results 2
! flhy3   FLow HYdrodynamic results 3
! flhywq  FLow HYdrodynamic results to Water Quality
! flrsta  FLow read or write of ReSTArt information
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flwres.pf,v $
! Revision 1.17  1999/03/15  14:19:34  kuipe_j
! improve writing Aggr-file
!
! Revision 1.16  1998/06/08  12:35:56  kuipe_j
! log added
!
! Revision 1.15  1997/11/04  14:17:32  kuipe_j
! Retention basin
!
! Revision 1.14  1997/06/17  11:26:36  kuipe_j
! output in history format
!
! Revision 1.12  1997/01/23  08:29:20  kuipe_j
! Make flow module robust
!
! Revision 1.11  1996/09/03  14:52:07  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.10  1996/04/12  13:04:27  kuipe_j
! headers, minor changes
!
! Revision 1.9  1996/04/11  08:23:58  kuipe_j
! Kalman module added
!
! Revision 1.8  1996/01/17  14:38:58  kuipe_j
! header update
!
! Revision 1.7  1996/01/16  15:01:28  kuipe_j
! Restart improvements
!
! Revision 1.6  1995/09/22  10:02:35  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:08  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:37:01  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:39  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:41  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:19  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  08:38:00  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:43:57  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nenhyc, nenstr, nenlat
   parameter (nenhyc=34, nenstr=4, nenlat=2)
   integer nhyman ,nhymap ,nhytim ,ngrid  ,juer   ,istep  ,nstep  ,&
   &ntmpgr ,ker    ,nstman ,nsttim ,nstru  ,ncontr ,nlaman ,&
   &nlatim ,nqlat  ,wqagst ,dlwqts ,nclstr ,ncllat ,nefhis ,&
   &lagstm ,nlags  ,Iwqin
   integer ncelfl(3)      ,itim  (2)      ,&
   &fd_nefis_res      ,&
   &fd_nefis_rst      ,&
   &fd_nefis_waq      ,&
   &hyrmap(nhymap) ,hyrtim(nhytim) ,hycpre(nhyman) ,&
   &strtim(nsttim) ,stcpre(nstman) ,&
   &lattim(nlatim) ,lacpre(nlaman) ,&
   &arexop(2)      ,arexcn(ngrid,2)
   integer flwini(*)
   real    theta  ,psi    ,curtim          ,g
   real    cp    (ngrid,4),&
   &waoft(ngrid,*) ,afwfqs(ngrid,8) ,rp    (ngrid,4),&
   &tmpgr (ngrid,ntmpgr)            ,&
   &qlat (*)       ,qaggr (ngrid,3) ,qlaggr(*)      ,&
   &conhis(5,*)    ,strhis(dmstrh,*),contrl(17,*)   ,&
   &buffer(nenhyc-1,ngrid)   ,qltpar(9,*)
   real    buflag(lagstm,nlags), grhis(*)
   double  precision  dt, hp(ngrid,3), qp(ngrid,3)
   logical first  ,writim ,wrirst,  lfilt, lgrwt
   character*40 gridnm(*), strunm(*), qlatnm(*)
!
!     Declaration of local variables
!
   integer fd_nefis_dum    ,i ,   j
   integer codhyc(nenhyc) , codstr(nenstr), codlat(nenlat)
   logical lwqin  ,inires , aggr, llog
!
   data (codhyc(i),i=1,nenhyc) /1 ,2 ,2 ,2 ,2 ,3 ,4 ,4 ,4 ,4 ,&
   &5 ,5 ,5 ,5 ,6 ,6 ,6 ,6 ,7 ,8 ,&
   &9 ,9 ,9 ,9 ,10,10,10,10,11,11,&
   &11,11,12,13/
   data (codstr(i),i=1,nenstr) /1,1,1,2                            /
   data (codlat(i),i=1,nenlat) /1,2                                /
!
   if ( lfilt ) then
      j = 2
   else
      j = 3
   endif
!
!     Generate pre codes
!
   if (first) then
      call resadm (nenhyc, codhyc, hycpre)
      call resadm (nenstr, codstr, stcpre)
      call resadm (nenlat, codlat, lacpre)
   endif
!
   if (nefhis.ge.1) then
!     NEFIS output
      call FLHY1 (fd_nefis_res ,nhyman ,nhymap ,nhytim ,ngrid  ,&
      &itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
      &hyrmap ,hyrtim ,hp(1,j),qp(1,j),afwfqs(1,7)    ,&
      &waoft(1,4)     ,waoft(1,3)     ,afwfqs(1,1)    ,&
      &cp(1,1),cp(1,2),waoft(1,1)     ,afwfqs(1,3)    ,&
      &ncelfl(1)      ,ncelfl(2)      ,hycpre         ,&
      &tmpgr(1,1)     ,ker            )
!
      call FLHY2 (fd_nefis_res ,nstman ,nsttim ,ngrid  ,itim   ,&
      &istep  ,nstep  ,first  ,writim ,juer   ,strtim ,&
      &nstru  ,strhis ,nclstr ,stcpre ,&
      &tmpgr(1,1)     ,ker    )
!
      call FLHY3 (fd_nefis_res ,nlaman ,nlatim ,ngrid  ,itim   ,&
      &istep  ,nstep  ,first  ,writim ,juer   ,lattim ,&
      &nqlat  ,qlat   ,ncllat ,lacpre ,tmpgr(1,1)     ,&
      &ker    ,qltpar ,strhis ,nstru  )
!
   endif
!
   if (nefhis.ne.1.and.dt.gt.0.0D0) then
!       HIS output
!       dt = negative for estuary morfology step, so no output
!
      call FLWRHI(hp(1,j), qp(1,j), ngrid  , itim   , flwini,&
      &strhis , nstru  , istep  , nstep  , dt    , qlat  ,&
      &nqlat  , hyrtim , nhytim , hycpre , afwfqs(1,7)   ,&
      &waoft(1,4)      , waoft(1,3)      , afwfqs(1,1)   ,&
      &cp(1,1), cp(1,2), waoft(1,1)      , afwfqs(1,3)   ,&
      &waoft(1,2)      , rp(1,1), rp(1,2), g     , nsttim,&
      &strtim , stcpre , nlatim , lattim , lacpre, hyrmap,&
      &nhymap , gridnm , strunm , qlatnm , buffer, qltpar)
   endif
!

   lwqin = Iwqin.gt.0
   if ( lwqin ) then
      call FLAGGR(istep  ,wqagst ,theta  ,ngrid  ,nqlat       ,&
      &qp(1,1),qp(1,3),afwfqs(1,5)    ,afwfqs(1,7) ,&
      &qlat   ,qaggr  ,qlaggr )
!
      aggr = mod(istep,wqagst) .eq. 0
      if ( first .or. aggr .or. istep.ge.nstep ) then
         if (Iwqin .eq. 1) then
!             Standard call
            call FLHYWQ  (fd_nefis_waq ,ngrid  ,nqlat  ,dlwqts ,&
            &abs(dt),itim   ,first  ,aggr   ,wqagst ,&
!                                                   Af(n+THETA)
            &writim ,istep  ,nstep  ,waoft(1,3)     ,&
!                                           At(n+1)
            &afwfqs(1,1)    ,waoft(1,4)     ,cp(1,1),&
            &psi    ,qaggr  ,&
!                                   Wt(n+1)
            &qlaggr ,waoft(1,2)     ,afwfqs(1,3)    ,&
            &juer   ,ker    )
         endif
      endif
   endif
   if ( wrirst ) then
      llog = .false.
      call FLRSTA (lwqin  ,nqlat  ,nstru  ,ncontr ,ngrid  ,itim   ,&
      &curtim ,juer   ,llog   ,.false., fd_nefis_rst,&
      &fd_nefis_dum ,hp(1,3),qp(1,3),contrl ,conhis ,&
      &strhis ,qaggr  ,qlaggr ,ncelfl(3)      ,inires ,&
      &arexop ,arexcn ,lagstm ,nlags  ,&
      &lgrwt  ,grhis  ,buflag ,ker    )
   endif
!
!     Set first to false
!
   first = .false.
!
end
