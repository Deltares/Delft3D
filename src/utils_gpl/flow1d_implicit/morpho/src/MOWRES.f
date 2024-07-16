      subroutine mowres (ngrid , nbran , maxlev, nmomap, nmotim, nmoman,
     +                   ntmpgr, itim  , istep , nstep , first , writim,
     +                   wrirst, juer  , fd_nefis_res, fd_nefis_rst,
     +                   mocpre, mormap, mortim, hlev  , sedtr , dissed,
     +                   branch, typcr , tmpgr , ncelmo, dt    , morini,
     +                   buffer, sectc , sectv , wft   , sumda , hlev0 ,
     +                   gridnm, nefhis, ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOWRES (MOrphology Write RESults)
c
c Module description: Write morphology results and restart information.
c
c                     This subroutine will start the result writing
c                     routine and the restart file writing routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 26 branch            P  -
c 16 dafdrs            P  -
c 18 dafdst            P  -
c 15 defdrs            P  -
c 17 defdst            P  -
c 23 dissed            P  -
c 11 first             O  True in case of first call.
c 20 hlev              P  -
c  9 istep             P  -
c  8 itim              P  -
c 14 juer              P  -
c 29 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 19 mocpre            P  -
c 20 mormap            P  -
c 21 mortim            P  -
c  2 nbran             I  Number of branches.
c 28 ncelmo            P  -
c  1 ngrid             I  Number of grid points in network.
c 23 nlev              P  -
c  6 nmoman            I  Number of main codes morphology module
c  4 nmomap            I  Number of entries in sedmap.
c  5 nmotim            I  Number of entries in sedtim.
c 10 nstep             P  -
c  7 ntmpgr            I  Number of scratch arrays with length ngrid
c                         that are packed in tmpgr.
c 24 sedtr             P  -
c 27 tmpgr             P  -
c 13 wrirst            I  True when restart info must be written.
c 12 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c momor   MOrphology MOrphology Results
c morsta  MOrphology read or write ReSTart information
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mowres.pf,v $
c Revision 1.8  1997/06/17  11:27:03  kuipe_j
c output in history format
c
c Revision 1.7  1997/02/17  10:23:19  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.6  1997/01/23  08:29:52  kuipe_j
c Make flow module robust
c
c Revision 1.5  1996/02/09  15:13:31  kuipe_j
c a.o. Restart improvements
c
c Revision 1.4  1996/01/16  15:01:40  kuipe_j
c Restart improvements
c
c Revision 1.3  1995/05/30  09:55:58  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:02  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:27  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/02/10  09:15:09  kuipe_j
c Some minor changes for beta release
c
c Revision 1.2  1993/11/26  15:33:03  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include sobek error codes
c
      include '..\include\errcod.i'
      include '..\include\sobdim.i'
c
c     Parameters
c
      integer        nentri  ,maxpar
      parameter     (nentri=6,maxpar=5)
      integer       ngrid , nbran  ,maxlev, nmomap, nmotim, nmoman,
     +              ntmpgr, istep , nstep , ker   , juer  , nefhis

      logical       first , writim, wrirst

      integer       fd_nefis_res, fd_nefis_rst,
     +              branch (4,nbran),  typcr(ngrid)  ,
     +              itim   (2),
     +              mocpre (nmoman),
     +              mormap (nmomap),
     +              mortim (nmotim),
     +              ncelmo (3)     ,
     +              morini (*)

      double precision      dt, hlev (ngrid,maxlev)
      real          wft(ngrid,maxlev),
     +              sedtr  (ngrid,*),
     +              dissed (4,nbran),
     +              tmpgr  (ngrid,ntmpgr),
     +              buffer (nentri+2*maxpar-1,ngrid),
     +              sectc  (ngrid,3),sectv(ngrid,dmsecv),sumda(ngrid),
     +              hlev0  (ngrid) 

      character*40  gridnm(*)
c
c     Local variables
c
      integer       i
      integer       ::     fd_nefis_dum = -1
      integer       code(nentri)
      logical       inires
c
      data          (code(i),i=1,nentri) / 1,1,1,1,2,3 /
c
      if (first) call resadm(nentri, code, mocpre)
c
      if     (nefhis.ge.1) then
c       NEFIS output
        call momor ( fd_nefis_res ,nmoman ,nmomap ,nmotim ,ngrid  ,
     +               nbran  ,itim   ,istep  ,nstep  ,first  ,writim ,
     +               juer   ,mormap ,mortim ,maxlev ,hlev   ,sedtr  ,
c                                     <ncelm>   <ncelh>
     +               dissed ,branch ,typcr  ,ncelmo(1),ncelmo(2)    ,
     +               mocpre ,sectc  ,sectv  ,wft    ,sumda  ,hlev0  ,
     +               tmpgr  ,ker    )
        if (ker .eq. fatal) goto 1000
c
      endif
c
      if (nefhis.ne.1) then
c       HIS output
c       dt = negative for estuary morfology step
c
        call mowrhh( nmoman, nmomap, nmotim, ngrid , nbran , itim   ,
     +               istep , nstep , mormap, mortim, maxlev, hlev   ,
     +               sedtr , dissed, branch, typcr , mocpre, abs(dt),
     +               morini, buffer, sectc , sectv , wft   , sumda  ,
     +               hlev0 , gridnm)

       
        call mowrhm( nmoman, nmomap, nmotim, ngrid , nbran , itim   ,
     +               istep , nstep , mormap, mortim, maxlev, hlev   ,
     +               sedtr , dissed, branch, typcr , mocpre, abs(dt),
     +               morini, buffer, sectc , sectv , wft   , sumda  ,
     +               hlev0 , gridnm)
      endif
c
c     Set first to false
c
      first = .false.
c
      if (wrirst) then
         call morsta ( ngrid , maxlev , itim   ,
     +                 hlev  , .false., fd_nefis_rst,
     +                 fd_nefis_dum, juer   , first  ,
c                      <ncelst>
     +                 ncelmo(3)      , inires , ker   , .false. )
c
      endif
c
 1000 continue
c
      end
