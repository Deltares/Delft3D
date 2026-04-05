subroutine KAPHYR(fd_nefis_res ,nphymn ,nkphmp ,nkphtm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
&kphmap ,kphtim ,pi     ,np     ,p2     ,&
&ncelm  ,ncelh  ,phycpr ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAPHYR (KAlman Predicted HYdraulic Results)
!
! Module description: Write the user selected Kalman module results to
!                     the result file. The results processed in this
!                     module are predicted covariances of water levels
!                     and discharges.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time. See [S-DD-002.1MR] for
!                     a specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 21 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 10 first             I  True in case of first call.
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 12 juer              P  -
! 22 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 13 kphmap(*)         I  Parameter list for prediction results of water
!                         levels and discharges of MAP block.
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 14 kphtim(*)         I  Parameter list for prediction results of water
!                         levels and discharges of HIST block.
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
! 19 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 18 ncelm             I  Actual cell number of a map block of result
!                         file.
!  6 ngrid             I  Number of grid points in network.
!  4 nkphmp            I  Number of entries in kphmap.
!  5 nkphtm            I  Number of entries in kphtim.
! 16 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  3 nphymn            I  Number of main codes of hydrodynamic prediction
!  9 nstep             I  Last time step number in simulation.
! 17 p2(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n (predicted
!                         values).
! 20 phycpr(nphymn)    I  phycpr(i) = index in block table (1...nentri)
!                         for main code i of the hydrodynamic prediction
! 15 pi                I  prediction interval (number of prediction steps)
! 11 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
! resadm  RESults; make ADMinistration for element selection
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
! yesmap  YES MAP results are written
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaphyr.pf,v $
! Revision 1.5  1999/03/15  15:52:03  kuipe_j
! tabs removed
!
! Revision 1.4  1996/12/03  07:59:07  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:26  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:05:13  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:46  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nphymn ,nkphmp ,nkphtm ,ngrid  ,juer   ,istep  ,&
   &nstep  ,ncelm  ,ncelh  ,ker    ,pi     ,np
   integer      fd_nefis_res, kphmap(nkphmp),&
   &kphtim(nkphtm) ,phycpr(nphymn),itim  (2)
   real         buf(ngrid)     ,p2(np,np)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=3)
   integer      errr  ,i ,j ,ie  ,nrerr, igr     ,nsk ,nlc ,lastcod
   integer      code  (nentri)   ,usrord(1),&
   &uindex(3)        ,ibuf(1)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog, new    ,newuit
!
!     Declaration of external functions
!
   integer      putiel, putrel ,flsdat
   logical      yesmap
   external     putiel, putrel ,flsdat ,yesmap
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (code(i) ,descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
   &1 ,'Prediction interval'              ,'HIS_pi ' ,'pi ', '-'    ,&
   &2 ,'Predicted water level covariance' ,'HIS_hcp' ,'hcp', 'm2'   ,&
   &3 ,'Predicted discharge covariance'   ,'HIS_qcp' ,'qcp', 'm6/s2'/
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KPHYD-MAP-GROUP' ,&
   &'KPHYD-HIS-GROUP' ,&
   &'KPHYD-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,phycpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KPHYD',&
      &nentri ,nkphmp ,nkphtm ,ngrid  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,phycpr ,kphmap ,kphtim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
!     Write Map results
!
   if (nkphmp .gt. 3) then
      if (yesmap(kphmap(1),kphmap(2),kphmap(3),istep)) then
         nrerr = ekamap
!
         call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('KPHYD',fd_nefis_res, grnamd ,1 ,writim ,&
         &kphtim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncelm
         uindex(2) = ncelm
         uindex(3) = 1
!
         do 50 i = 4,nkphmp,2
            ie = phycpr(kphmap(i))+kphmap(i+1)
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
            name  = 'MAP'//nameel(ie)(4:)
            goto (10,20,30) , ie
10          continue
            goto 40
20          continue
            do 25 igr = 1, ngrid
               buf(igr) = p2(igr,igr)
25          continue
            goto 40
30          continue
            do 35 igr = 1, ngrid
               buf(igr) = p2(ngrid+igr,ngrid+igr)
35          continue
            goto 40
40          continue
            if (ie .eq. 1) then
               ibuf(1) = pi
               errr = putiel (fd_nefis_res, grnamm ,name   ,&
               &uindex ,usrord ,ibuf   )
            else
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex ,usrord ,buf    )
            endif
            if (errr.ne.0) goto 1000
50       continue
      endif
   endif
!
!     Write History results.
!
   nlc = kphtim(1)
   new = mod(nkphtm-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nkphtm .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(kphtim(nlc+2),kphtim(nlc+3),kphtim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (kphtim(1).gt.0 .and. .not. new) .or. newuit ) then
         nrerr = ekahis
!
         call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('KPHYD',fd_nefis_res, grnamd ,2 ,writim ,&
         &kphtim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncelh
         uindex(2) = ncelh
         uindex(3) = 1
!
         do 110 i = kphtim(1)+2+nsk,nkphtm,2
            ie = phycpr(kphtim(i))+kphtim(i+1)
            goto (70,80,90) ,ie
70          continue
            goto 100
80          continue
            do 85 j=1,kphtim(1)
               igr = kphtim(j+1)
               buf(j) = p2(igr,igr)
85          continue
            goto 100
90          continue
            do 95 j=1,kphtim(1)
               igr = kphtim(j+1)
               buf(j) = p2(igr+ngrid,igr+ngrid)
95          continue
            goto 100
100         continue
            if (ie .eq. 1) then
               ibuf(1) = pi
               errr = putiel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex ,usrord ,ibuf   )
            else
               errr = putrel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex ,usrord ,buf    )
            endif
            if (errr.ne.0) goto 1000
110      continue
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = ekaeoo
      llog  = .true.
!
      call resdes ('KPHYD' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kphtim ,ncelm  ,ncelh  ,errr   )
      if (errr.ne.0) goto 1000
!
      errr = flsdat (fd_nefis_res)
      if (errr.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') errr
   call error (juer ,'KAPHYR @'//txt//'@' ,nrerr ,ker)
!
end
