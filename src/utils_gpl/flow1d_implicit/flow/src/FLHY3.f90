subroutine flhy3 (fd_nefis_res ,nlaman ,nlatim ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,&
&juer   ,lattim ,nqlat  ,qlat   ,ncllat ,&
&lacpre ,buf    ,ker    ,qltpar ,strhis ,&
&nstru  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHY3 (FLow HYdrodynamic results 3)
!
! Module description: Subroutine FLHY3 writes the third part of the user
!                     selected flow results to the result file. The
!                     result file is processed by the User Interface.
!
!                     In subroutine FLHY3 the user selected water flow
!                     results at user supplied locations and time levels
!                     (System Specifications for 1D Modelling System
!                     Front End) will be stored on the result file. The
!                     stored data can be processed further by the User
!                     Interface.
!
!                     The user can select functions of place or func-
!                     tions of time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
!  9 first             I  True in case of first call.
!  7 istep             I  Current time step number (t(n+1)).
!  6 itim              P  -
! 11 juer              P  -
! 18 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 16 lacpre(nlaman)    I  lacpre(i) = index in block table (1...nentri)
!                         for main code i of the lateral discharges
!                         results.
! 12 lattim(nlatim)    I  Parameter list for HIST block with hydrodyna-
!                         mic lateral discharge results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nlatim) = Report parameter n sub code
! 15 ncllat            I  Actual cell number of a history block for the
!                         lateral discharges results at the NEFIS re-
!                         sultfile.
!  5 ngrid             I  Number of grid points in network.
!  3 nlaman            I  Number of main codes of lateral discharges
!                         results.
!  4 nlatim            I  Number of entries in lattim.
! 13 nqlat             P  -
!  8 nstep             I  Last time step number in simulation.
!    nstru             P  -
! 14 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
! 10 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhy3.pf,v $
! Revision 1.8  1999/03/15  14:24:44  kuipe_j
! Bug fix lateral discharge on t=0
!
! Revision 1.7  1997/11/04  14:17:23  kuipe_j
! Retention basin
!
! Revision 1.6  1997/06/17  11:26:34  kuipe_j
! output in history format
!
! Revision 1.5  1996/12/02  15:41:26  kuipe_j
! dimension for histories improvred
!
! Revision 1.4  1996/09/03  14:51:59  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.3  1995/11/21  14:16:21  kuipe_j
! proper initialization of error code
!
! Revision 1.2  1995/05/30  09:55:05  hoeks_a
! Minor changes
!
! Revision 1.1  1995/04/13  07:07:47  hoeks_a
! Initial check-in
!
! Revision 1.3  1995/03/08  09:07:27  kuipe_j
! Call to resdes improved
!
! Revision 1.2  1993/11/26  15:30:59  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nlaman ,nlatim ,ngrid  ,juer   ,istep   ,&
   &nstep  ,nqlat  ,nceld  ,ncllat ,ker
   integer      fd_nefis_res, itim(2) ,&
   &lattim(nlatim) ,lacpre(nlaman)
   integer      nstru
   real         qlat(*),buf(ngrid)
   real         qltpar(9,*)   ,strhis(13,nstru)
   logical      first   ,writim
!
!     Declaration of local variables
!
   integer      nentri ,nlamap
   parameter   (nentri=2 ,nlamap=1)
   integer      latmap(nlamap)
   integer      errr  ,i ,j ,ie  ,nrerr ,nsk ,nlc, lastcod
   integer      usrord(1),&
   &uindex(3)
   integer      istat ,istru
   character*16 grnamm          ,grnamh          ,grnamd
   character*16 nameel(nentri),quanel(nentri),unitel(nentri),&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*9  txt
   logical      llog, new       ,newuit
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
!      1
   &'Lateral discharge'               ,'HIS_Qlat' ,'Qlat','m3/s',&
   &'Water level in retention area'   ,'HIS_Hlat' ,'Hlat','m' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'UNUSED' ,&
   &'FLLAT-HIS-GROUP' ,&
   &'FLLAT-DES-GROUP' /
!
   latmap(1) = 0
   lastcod   = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = eflboo
!
      call resini (fd_nefis_res ,grnamd ,grnamm ,grnamh ,'FLLAT',&
      &nentri ,nlamap ,nlatim ,nqlat  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,lacpre ,latmap ,lattim ,&
      &nceld  ,ncllat ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
!     Write History results.
!
   nlc = lattim(1)
   new = mod(nlatim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nlatim .gt. 1+nsk ) then
      if (new) then
         newuit = yesmap(lattim(nlc+2),lattim(nlc+3),lattim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ((lattim(1) .gt.0  .and.  .not. new)  .or. newuit) then
         nrerr = eflhis
!
         call restim (fd_nefis_res ,grnamh ,1  ,itim  ,ncllat ,&
         &nameac ,errr   )
         if (errr.ne.0) goto 1000
!
         call resdes ('FLLAT',fd_nefis_res ,grnamd ,2 ,writim ,&
         &lattim ,nceld  ,ncllat ,errr   )
         if (errr.ne.0) goto 1000
!
         uindex(1) = ncllat
         uindex(2) = ncllat
         uindex(3) = 1
!
         do 410 i = lattim(1)+2+nsk,nlatim,2
            ie = lacpre(lattim(i)) + lattim(i+1)
            if (ie .eq. 1) then
               do 225 j=1,lattim(1)
                  buf(j) = qlat(lattim(j+1))
                  if (buf(j) .gt. 1.1e+20) buf(j) = 0.
225            continue
               errr = putrel (fd_nefis_res ,grnamh ,nameel(ie) ,&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
            else if (ie .eq. 2) then
               do 230 j=1,lattim(1)
                  istat = lattim(j+1)
                  if (INT(qltpar(2,istat)).eq.cqlret) then
                     istru = MOD(INT(qltpar(9,istat)), 1000)
                     buf(j) = strhis(13,istru)
                  else
                     buf(j) = 0.
                  endif
230            continue
               errr = putrel (fd_nefis_res ,grnamh ,nameel(ie) ,&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
            endif
410      continue
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = efleoo
      llog  = .true.
!
      call resdes ('FLLAT' ,fd_nefis_res ,grnamd ,2 ,llog ,&
      &lattim ,nceld  ,ncllat ,errr   )
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
   call error (juer ,'FLHY3 @'//txt//'@' ,nrerr ,ker)
!
end
