subroutine sawres (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid  ,&
&nsamap ,nsatim ,nsaman ,ntmpgr ,itim   ,&
&curtim ,istep  ,nstep  ,first  ,writim ,&
&wrirst, juer, fd_nefis_res, fd_nefis_rst,&
&sacpre ,salmap ,saltim ,csa    ,&
&csd    ,disgr  ,cdcdx  ,thasca ,mouqpu ,&
&thcsum ,timout ,sbdscr ,sbdpar ,rho    ,&
&tmpgr  ,ncelsa ,ker    ,dt     ,salini ,&
&buffer ,gridnm ,nefhis                 )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAWRES (SAlt Write RESults)
!
! Module description: Write salt results and restart information.
!
!                     This subroutine will call the result writing rou-
!                     tine and the restart file writing routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 cdcdx             P  -
! 24 csa               P  -
! 25 csd               P  -
! 18 dafdrs            P  -
! 20 dafdst            P  -
! 17 defdrs            P  -
! 19 defdst            P  -
! 26 disgr             P  -
!  1 dsopt             P  -
! 13 first             O  True in case of first call.
! 11 istep             P  -
! 10 itim              P  -
! 11 curtim            P  -
! 16 juer              P  -
! 36 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 29 mouqpu            P  -
!  3 nboun             I  Number of boundary nodes.
!  4 nbran             I  Number of branches.
! 35 ncelsa            P  -
!  5 ngrid             I  Number of grid points in network.
!  2 nmouth            P  -
!  8 nsaman            I  Number of main codes of salt results.
!  6 nsamap            I  Number of entries in salmap.
!  7 nsatim            I  Number of entries in saltim.
! 12 nstep             P  -
!  9 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
! 33 rho               P  -
! 21 sacpre            P  -
! 22 salmap            P  -
! 23 saltim            P  -
! 32 sbdscr            P  -
! 32 sbdpar            P  -
! 28 thasca            P  -
! 30 thcsum            P  -
! 31 timout            P  -
! 34 tmpgr             P  -
! 15 wrirst            I  True when restart info must be written.
! 14 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! sarsta  SAlt read or write of ReSTArt information
! sasar   SAlt SAlt Results
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
! $Log: sawres.pf,v $
! Revision 1.8  1997/06/18  07:52:11  kuipe_j
! Remove unreferenced vars
!
! Revision 1.7  1997/06/17  11:27:10  kuipe_j
! output in history format
!
! Revision 1.6  1996/12/02  15:31:44  kuipe_j
! Salt restart improved
!
! Revision 1.5  1996/02/09  15:13:33  kuipe_j
! a.o. Restart improvements
!
! Revision 1.4  1996/01/16  15:01:46  kuipe_j
! Restart improvements
!
! Revision 1.3  1995/05/30  09:56:19  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:24  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:06  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/02/10  09:15:25  kuipe_j
! Some minor changes for beta release
!
! Revision 1.2  1993/11/26  15:34:13  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nentri
   parameter (nentri=6)
   integer dsopt ,nmouth  ,nboun  ,nbran    ,ngrid     ,nsamap    ,&
   &nsatim,nsaman  ,ntmpgr ,juer     ,istep     ,nstep     ,&
   &ker   ,nefhis
   integer fd_nefis_res, fd_nefis_rst,&
   &salmap(nsamap)  ,saltim(nsatim)  ,itim  (2) ,&
   &sacpre(nsaman)  ,ncelsa(3)
   integer salini(*)
   real    csa   (ngrid,2) ,csd   (ngrid,2) ,rho  (ngrid),&
   &cdcdx (ngrid,3) ,disgr (ngrid)   ,tmpgr(ngrid,ntmpgr)  ,&
   &thasca(3    )   ,mouqpu(3,0:2,*) ,curtim               ,&
   &thcsum(2,nbran) ,timout(2,*)     ,sbdscr(3,nboun)      ,&
   &sbdpar(5,nboun) ,buffer(nentri,ngrid)
   double precision dt
   character*40     gridnm(*)

   logical first ,writim   ,wrirst
!
!     Declaration of local variables
!
   integer                 :: fd_nefis_dum = -1
   integer code(nentri)
   logical inires
!
   include '..\include\errcod.i'
!
   data (code(i),i=1,nentri) / 1,2,3,4,5,6/
!
   ker = ok
!
   if (first) call resadm (nentri ,code   ,sacpre )
!
   if (nefhis.ge.1) then
!     NEFIS output
      call sasar (fd_nefis_res, nsaman  ,nsamap ,nsatim   ,ngrid    ,&
      &itim   ,istep  ,nstep   ,first  ,writim   ,juer     ,&
!                                <csa2>           <ncelm>   <ncelh>
      &salmap ,saltim ,csa(1,2),disgr ,ncelsa(1),ncelsa(2),&
      &sacpre ,rho    ,tmpgr(1,1)      ,ker    )
!
   endif
!
   if (nefhis.ne.1.and.dt.gt.0.0D0) then
!     HIS output
!     dt = negative for estuary morfology step, so no output
!

      call sawrhi(nsaman ,nsamap ,nsatim   ,ngrid  ,itim   ,istep  ,&
!                                        <csa2>
      &nstep  ,salmap ,saltim ,csa(1,2) ,disgr  ,&
      &sacpre ,rho    ,dt       ,salini ,buffer ,gridnm )
   endif
   if (ker .eq. fatal) goto 1000
!
!     Set first to false
!
   first = .false.
!
   if (wrirst) then
      call sarsta (dsopt  ,nmouth ,nboun  ,nbran  ,ngrid   ,itim    ,&
      &curtim ,juer   ,first  ,.false.,&
      &fd_nefis_rst, fd_nefis_dum,&
!                       <csa2>         <csd2>
      &csa(1,2)       ,csd(1,2)       ,&
!                     <cdcdx0>        <cdcdx1>        <cdcdx2>
      &cdcdx(1,1)     ,cdcdx(1,2)     ,cdcdx(1,3)       ,&
      &thasca ,mouqpu ,thcsum ,&
!                                     <ncelst>
      &timout ,sbdscr ,sbdpar ,ncelsa(3) ,inires  ,ker  )
   endif
!
1000 continue
end
