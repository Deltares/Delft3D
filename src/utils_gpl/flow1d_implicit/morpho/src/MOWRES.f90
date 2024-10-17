subroutine mowres (ngrid , nbran , maxlev, nmomap, nmotim, nmoman,&
&ntmpgr, itim  , istep , nstep , first , writim,&
&wrirst, juer  , fd_nefis_res, fd_nefis_rst,&
&mocpre, mormap, mortim, hlev  , sedtr , dissed,&
&branch, typcr , tmpgr , ncelmo, dt    , morini,&
&buffer, sectc , sectv , wft   , sumda , hlev0 ,&
&gridnm, nefhis, ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOWRES (MOrphology Write RESults)
!
! Module description: Write morphology results and restart information.
!
!                     This subroutine will start the result writing
!                     routine and the restart file writing routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 26 branch            P  -
! 16 dafdrs            P  -
! 18 dafdst            P  -
! 15 defdrs            P  -
! 17 defdst            P  -
! 23 dissed            P  -
! 11 first             O  True in case of first call.
! 20 hlev              P  -
!  9 istep             P  -
!  8 itim              P  -
! 14 juer              P  -
! 29 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 19 mocpre            P  -
! 20 mormap            P  -
! 21 mortim            P  -
!  2 nbran             I  Number of branches.
! 28 ncelmo            P  -
!  1 ngrid             I  Number of grid points in network.
! 23 nlev              P  -
!  6 nmoman            I  Number of main codes morphology module
!  4 nmomap            I  Number of entries in sedmap.
!  5 nmotim            I  Number of entries in sedtim.
! 10 nstep             P  -
!  7 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 24 sedtr             P  -
! 27 tmpgr             P  -
! 13 wrirst            I  True when restart info must be written.
! 12 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! momor   MOrphology MOrphology Results
! morsta  MOrphology read or write ReSTart information
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mowres.pf,v $
! Revision 1.8  1997/06/17  11:27:03  kuipe_j
! output in history format
!
! Revision 1.7  1997/02/17  10:23:19  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.6  1997/01/23  08:29:52  kuipe_j
! Make flow module robust
!
! Revision 1.5  1996/02/09  15:13:31  kuipe_j
! a.o. Restart improvements
!
! Revision 1.4  1996/01/16  15:01:40  kuipe_j
! Restart improvements
!
! Revision 1.3  1995/05/30  09:55:58  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:02  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:27  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/02/10  09:15:09  kuipe_j
! Some minor changes for beta release
!
! Revision 1.2  1993/11/26  15:33:03  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include sobek error codes
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Parameters
!
   integer        nentri  ,maxpar
   parameter     (nentri=6,maxpar=5)
   integer       ngrid , nbran  ,maxlev, nmomap, nmotim, nmoman,&
   &ntmpgr, istep , nstep , ker   , juer  , nefhis

   logical       first , writim, wrirst

   integer       fd_nefis_res, fd_nefis_rst,&
   &branch (4,nbran),  typcr(ngrid)  ,&
   &itim   (2),&
   &mocpre (nmoman),&
   &mormap (nmomap),&
   &mortim (nmotim),&
   &ncelmo (3)     ,&
   &morini (*)

   double precision      dt, hlev (ngrid,maxlev)
   real          wft(ngrid,maxlev),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &tmpgr  (ngrid,ntmpgr),&
   &buffer (nentri+2*maxpar-1,ngrid),&
   &sectc  (ngrid,3),sectv(ngrid,dmsecv),sumda(ngrid),&
   &hlev0  (ngrid)

   character*40  gridnm(*)
!
!     Local variables
!
   integer       i
   integer       ::     fd_nefis_dum = -1
   integer       code(nentri)
   logical       inires
!
   data          (code(i),i=1,nentri) / 1,1,1,1,2,3 /
!
   if (first) call resadm(nentri, code, mocpre)
!
   if     (nefhis.ge.1) then
!       NEFIS output
      call momor ( fd_nefis_res ,nmoman ,nmomap ,nmotim ,ngrid  ,&
      &nbran  ,itim   ,istep  ,nstep  ,first  ,writim ,&
      &juer   ,mormap ,mortim ,maxlev ,hlev   ,sedtr  ,&
!                                     <ncelm>   <ncelh>
      &dissed ,branch ,typcr  ,ncelmo(1),ncelmo(2)    ,&
      &mocpre ,sectc  ,sectv  ,wft    ,sumda  ,hlev0  ,&
      &tmpgr  ,ker    )
      if (ker .eq. fatal) goto 1000
!
   endif
!
   if (nefhis.ne.1) then
!       HIS output
!       dt = negative for estuary morfology step
!
      call mowrhh( nmoman, nmomap, nmotim, ngrid , nbran , itim   ,&
      &istep , nstep , mormap, mortim, maxlev, hlev   ,&
      &sedtr , dissed, branch, typcr , mocpre, abs(dt),&
      &morini, buffer, sectc , sectv , wft   , sumda  ,&
      &hlev0 , gridnm)


      call mowrhm( nmoman, nmomap, nmotim, ngrid , nbran , itim   ,&
      &istep , nstep , mormap, mortim, maxlev, hlev   ,&
      &sedtr , dissed, branch, typcr , mocpre, abs(dt),&
      &morini, buffer, sectc , sectv , wft   , sumda  ,&
      &hlev0 , gridnm)
   endif
!
!     Set first to false
!
   first = .false.
!
   if (wrirst) then
      call morsta ( ngrid , maxlev , itim   ,&
      &hlev  , .false., fd_nefis_rst,&
      &fd_nefis_dum, juer   , first  ,&
!                      <ncelst>
      &ncelmo(3)      , inires , ker   , .false. )
!
   endif
!
1000 continue
!
end
