subroutine KAFGAR(fd_nefis_res, nfgamn ,nkfgmp ,itim   ,&
&istep  ,nstep  ,first  ,writim ,lfilt  ,juer   ,&
&kfgmap ,ncelm  ,fgacpr ,np     ,nsamp  ,&
&kgain  ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFGAR (KAlman Filter kalman GAin Results)
!
! Module description: Write the Kalman gain matrix to the result file.
!
!                     The stored data can be processed further by the
!                     User Interface. It is in map format but as it is a
!                     2D-array, it cannot be used in the same way as
!                     other map functions. See [S-DD-002.1MR] for a
!                     specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 14 fgacpr(nfgamn)    I  fgacpr(i) = index in block table (1...nentri)
!                         for main code i of the Kalman gain.
!  8 first             I  True in case of first call.
!  6 istep             I  Current time step number (t(n+1)).
!  5 itim              P  -
! 11 juer              P  -
! 18 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 12 kfgmap(*)         I  Parameter list for Kalman gain (MAP block).
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
! 17 kgain             P  -
! 10 lfilt             I  = True if a filter step must be performed.
! 13 ncelm             I  Actual cell number of a map block of result
!                         file.
!  3 nfgamn            I  Number of main codes of the Kalman gain.
!  4 nkfgmp            I  Number of entries in kfgmap.
! 15 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 16 nsamp             I  Number of hydrodynamic samples (measurements)
!  7 nstep             I  Last time step number in simulation.
!  9 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
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
! $Log: kafgar.pf,v $
! Revision 1.3  1996/12/03  07:59:03  kuipe_j
! dimension of element names improved
!
! Revision 1.2  1996/04/12  13:04:50  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:27  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nfgamn ,nkfgmp ,juer   ,istep  ,&
   &nstep  ,ncelm  ,np     ,nsamp  ,ker
   integer      fd_nefis_res, kfgmap(nkfgmp),&
   &fgacpr(nfgamn) ,itim  (2)
   real         kgain(np*nsamp)
   logical      first   ,writim,lfilt
!
!     Declaration of local variables
!
   integer      nentri, nkfgtm
   parameter   (nentri=1 ,nkfgtm=1)
   integer      errr  ,i ,ie ,nrerr, nceld, lastcod
   integer      kfgtim(nkfgtm)  ,code  (nentri)  ,usrord(1)      ,&
   &uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
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
   &1 ,'Kalman gain' ,'HIS_Kg' ,'Kg', '-' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KFGAN-MAP-GROUP' ,&
   &'UNUSED'          ,&
   &'KFGAN-DES-GROUP' /
!
   kfgtim(1) = 0
   lastcod   = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,fgacpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFGAN',&
      &nentri ,nkfgmp ,nkfgtm ,np*nsamp       ,itim   ,&
      &nameel ,quanel ,unitel ,descel ,fgacpr ,kfgmap ,&
      &kfgtim ,ncelm  ,nceld  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfgmp .gt. 3) then
         if (yesmap(kfgmap(1),kfgmap(2),kfgmap(3),istep)) then
            nrerr = ekamap
!
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFGAN',fd_nefis_res, grnamd ,1 ,writim ,&
            &kfgtim ,ncelm  ,nceld  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
!
            do 20 i = 4,nkfgmp,2
               ie = fgacpr(kfgmap(i))+kfgmap(i+1)
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,kgain  )
               if (errr.ne.0) goto 1000
20          continue
         endif
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
      call resdes ('KFGAN' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kfgtim ,ncelm  ,nceld  ,errr   )
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
   call error (juer ,'KAFGAR @'//txt//'@' ,nrerr ,ker)
!
end
