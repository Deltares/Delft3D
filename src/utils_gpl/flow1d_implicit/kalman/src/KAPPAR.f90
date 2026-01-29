subroutine KAPPAR(fd_nefis_res, nppamn ,nkppmp ,nkpptm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,juer   ,&
&kppmap ,kpptim ,pi     ,np     ,p2     ,nkapar ,&
&ncelm  ,ncelh  ,ppacpr ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAPPAR (KAlman Predicted PArameter Results)
!
! Module description: Write the user selected Kalman module results to
!                     the result file. The results processed in this
!                     module are predicted covariances of the correction
!                     parameters.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time. See [S-DD-002.1MR] for
!                     a specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 22 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 10 first             I  True in case of first call.
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 12 juer              P  -
! 23 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 13 kppmap(*)         I  Parameter list for prediction results of para-
!                         meters MAP block.
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 14 kpptim(*)         I  Parameter list for prediction results of para-
!                         meters HIST block.
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
! 20 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 19 ncelm             I  Actual cell number of a map block of result
!                         file.
!  6 ngrid             I  Number of grid points in network.
! 18 nkapar            I  = nnf + nnmu + 1
!  4 nkppmp            I  Number of entries in kppmap.
!  5 nkpptm            I  Number of entries in kpptim.
! 16 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  3 nppamn            I  Number of main codes of parameter prediction
!  9 nstep             I  Last time step number in simulation.
! 17 p2(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n (predicted
!                         values).
! 15 pi                I  prediction interval (number of prediction steps)
! 21 ppacpr(nppamn)    I  ppacpr(i) = index in block table (1...nentri)
!                         for main code i of the parameter prediction
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
! $Log: kappar.pf,v $
! Revision 1.5  1999/03/15  15:52:06  kuipe_j
! tabs removed
!
! Revision 1.4  1996/12/03  07:59:08  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:27  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:05:15  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:48  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nppamn ,nkppmp ,nkpptm ,ngrid  ,juer   ,istep  ,&
   &nstep  ,ncelm  ,ncelh  ,ker    ,pi     ,np     ,&
   &nkapar
   integer      fd_nefis_res, kppmap(nkppmp),&
   &kpptim(nkpptm) ,ppacpr(nppamn),itim  (2)
   real         p2(np,np)      , buf(nkapar)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=2)
   integer      errr  ,i ,j ,k ,ie ,nrerr, igr, nlc, nsk, lastcod
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
   &1 ,'Prediction interval'            ,'HIS_pi ' ,'pi ', '-' ,&
   &2 ,'Predicted parameter covariance' ,'HIS_pcp' ,'pcp', '-' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KPPAR-MAP-GROUP' ,&
   &'KPPAR-HIS-GROUP' ,&
   &'KPPAR-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,ppacpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KPPAR',&
      &nentri ,nkppmp ,nkpptm ,nkapar ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,ppacpr ,kppmap ,kpptim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
!     Write Map results
!
   if (nkppmp .gt. 3) then
      if (yesmap(kppmap(1),kppmap(2),kppmap(3),istep)) then
         nrerr = ekamap
!
         call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('KPPAR',fd_nefis_res, grnamd ,1 ,writim ,&
         &kpptim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncelm
         uindex(2) = ncelm
         uindex(3) = 1
!
         do 40 i = 4,nkppmp,2
            ie = ppacpr(kppmap(i))+kppmap(i+1)
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
            name  = 'MAP'//nameel(ie)(4:)
            goto (10,20) , ie
10          continue
            goto 30
20          continue
            k = 1
            do 25 j = ngrid*2+1, np
               buf(k) = p2(j,j)
               k = k + 1
25          continue
            goto 30
30          continue
            if (ie .eq. 1) then
               ibuf(1) = pi
               errr = putiel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,ibuf   )
            else
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,buf    )
            endif
            if (errr.ne.0) goto 1000
40       continue
      endif
   endif
!
!     Write History results.
!
   nlc = kpptim(1)
   new = mod(nkpptm-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nkpptm .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(kpptim(nlc+2),kpptim(nlc+3),kpptim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (kpptim(1).gt.0 .and. .not. new) .or. newuit ) then
         nrerr = ekahis
!
         call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('KPPAR',fd_nefis_res, grnamd ,2 ,writim ,&
         &kpptim ,ncelm  ,ncelh  ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncelh
         uindex(2) = ncelh
         uindex(3) = 1
!
         do 80 i = kpptim(1)+2+nsk,nkpptm,2
            ie = ppacpr(kpptim(i))+kpptim(i+1)
            goto (50,60) ,ie
50          continue
            goto 70
60          continue
            do 65 j=1,kpptim(1)
               igr = kpptim(j+1) + 2*ngrid
               buf(j) = p2(igr,igr)
65          continue
            goto 70
70          continue
            if (ie .eq. 1) then
               ibuf(1) = pi
               errr = putiel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex  ,usrord ,ibuf   )
            else
               errr = putrel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex  ,usrord ,buf    )
            endif
            if (errr.ne.0) goto 1000
80       continue
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
      call resdes ('KPPAR' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kpptim ,ncelm  ,ncelh  ,errr   )
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
   call error (juer ,'KAPPAR @'//txt//'@' ,nrerr ,ker)
!
end
