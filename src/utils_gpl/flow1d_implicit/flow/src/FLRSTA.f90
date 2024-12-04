subroutine FLRSTA(lwqin  ,nqlat  ,nstru  ,ncontr ,ngrid  ,itim   ,&
&curtim ,juer   ,first  ,newres ,fd_nefis_rst ,&
&fd_nefis_new ,h2     ,q2     ,contrl ,conhis ,&
&strhis ,qaggr  ,qlaggr ,ncelst ,inires ,arexop ,&
&arexcn ,lagstm ,nlags  ,&
&lgrwt  ,grhis  ,buflag ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLRSTA (FLow read or write of ReSTArt information)
!
! Module description: Reading or writing of restart information.
!
!                     A parameter determines if reading or writing takes
!                     place. For an initial run no reading will be done
!                     as there is no information on the file. In that
!                     case only group definitions will be made. When the
!                     user restarts a simulation run from a specific
!                     point of time the saved restart information from a
!                     previous run will be read. Writing can be done at
!                     some interval in time. Previously in the same run
!                     written information will be lost.
!
!                     Pre condition: The NEFIS data and definition files
!                     are opened.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 25 arexcn            P  -
! 24 arexop            P  -
! 18 conhis            P  -
! 17 contrl            P  -
!  7 curtim            P  -
! 14 dafdrn            P  -
! 12 dafdst            P  -
! 13 defdrn            P  -
! 11 defdst            P  -
!  9 first             I  True in case of first call.
! 15 h2                P  -
! 23 inires            I  True when no restart info of this module has
!                         been written before.
!  6 itim              P  -
!  8 juer              P  -
! 26 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 lwqin             P  -
! 22 ncelst            IO Actual cell number of a restart block of the
!                         restart file.
!  4 ncontr            P  -
! 10 newres            I  true, if a new restart file will be made
!  5 ngrid             I  Number of grid points in network.
!  2 nqlat             P  -
!  3 nstru             P  -
! 16 q2                P  -
! 20 qaggr             P  -
! 21 qlaggr            P  -
! 19 strhis            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! fldfst  FLow DeFine group for reSTart information
! flrest  FLow REading of reSTart information
! flwrst  FLow WRiting of reSTart information
! statim  Search resTArt TIMe point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flrsta.pf,v $
! Revision 1.13  1999/07/23  15:10:21  kuipe_j
! improve restart
!
! Revision 1.12  1998/06/18  13:32:16  kuipe_j
! Bug in resart flag solved
!
! Revision 1.11  1998/06/08  12:29:49  kuipe_j
! time lag hydr controller
!
! Revision 1.10  1996/12/02  15:41:27  kuipe_j
! dimension for histories improvred
!
! Revision 1.9  1996/01/17  14:38:48  kuipe_j
! header update
!
! Revision 1.8  1996/01/16  15:01:24  kuipe_j
! Restart improvements
!
! Revision 1.7  1995/10/18  08:59:26  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.6  1995/09/22  10:02:13  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:03  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:36:52  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:26  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:26  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:06  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:32  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
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
   integer      nqlat  ,nstru   ,ncontr ,ngrid  ,ncelst ,&
   &lagstm ,nlags   ,juer   ,ker
   integer      fd_nefis_rst, fd_nefis_new, itim  (2)  ,&
   &arexop(*)       ,arexcn(ngrid,2)
   real         curtim
   real         conhis(5,*),strhis(dmstrh,*),contrl(17,*)   ,&
   &buflag(lagstm,nlags)            ,&
   &qaggr(ngrid,3)  ,qlaggr(*)      , grhis(*)
   logical      lwqin  ,first   ,inires, newres ,lgrwt
   double precision h2(ngrid), q2(ngrid)

!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=10)
   integer      errr  ,nrerr ,i
   integer      ndim  (nentri)
   character*16 grnamf
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      inidum
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Definition of elements.
!
   data (ndim(i)  ,descel(i)   ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
   &1,'Restart time step'              ,'ISTEP'  ,'t'  ,'-'    ,&
   &1,'Water levels on t=n+1'          ,'H2'     ,'h'  ,'m'    ,&
   &1,'Discharges on t=n+1'            ,'Q2'     ,'Q'  ,'m3/s' ,&
   &2,'Structure history'              ,'STRHIS' ,'<>' ,'-'    ,&
   &2,'Control history'                ,'CONHIS' ,'<>' ,'-'    ,&
   &2,'Aggregated flows WQ-interface'  ,'QAGGR'  ,'Q'  ,'m3/s' ,&
   &1,'Aggregated lateral discharges'  ,'QLAGGR' ,'Q'  ,'m3/s' ,&
   &2,'Status variable for extra area' ,'AREXCN' ,'<>' ,'-'    ,&
   &2,'Time lag in discharges        ' ,'BUFLAG' ,'<>' ,'-'    ,&
   &3,'Groundwater history           ' ,'GRHIS'  ,'<>' ,'-'    /
!
   data  grnamf   /&
   &'FLOW-RES-GROUP' /
!
   errr = 0
!
   if (first) then
!
!        Determine if it is an initial run or a restart run for this
!        module. In the latter case restart information must exist
!        already.
!
      nrerr = eflbor
      call FLDFST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr ,&
      &nqlat  ,ngrid  ,lwqin  ,arexop ,ndim   ,nameel ,&
      &quanel ,unitel ,descel ,.not.newres    ,inires ,&
      &nameac ,lagstm ,nlags  ,lgrwt  ,errr   )
      if (errr.ne.0) goto 1000
!
      if (inires) then
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = eflbor
            call FLDFST (fd_nefis_new ,grnamf ,nentri ,nstru  ,&
            &ncontr ,nqlat  ,ngrid  ,lwqin  ,arexop ,&
            &ndim   ,nameel ,quanel ,unitel ,descel ,&
            &.true. ,inidum ,nameac ,lagstm ,nlags  ,&
            &lgrwt  ,errr   )
            if (errr.ne.0) goto 1000
         endif
         ncelst = 1
      else
!
!           Read from restart file. Select proper time step first.
!
         nrerr = eflrrd
         call STATIM (fd_nefis_rst ,grnamf ,nameel(1),itim,ncelst,&
         &errr   )
         if (errr.ne.0) goto 1000
!
         call FLREST (fd_nefis_rst ,grnamf ,nentri ,nstru ,ncontr ,&
         &ngrid  ,lwqin  ,ncelst ,nameel ,h2     ,q2    ,&
         &conhis ,strhis ,qaggr  ,qlaggr ,arexop ,arexcn,&
         &lagstm ,nlags  ,buflag ,lgrwt  ,grhis  ,errr  )
         if (errr.ne.0) goto 1000
!
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = eflbor
            call FLDFST (fd_nefis_new ,grnamf ,nentri ,nstru  ,&
            &ncontr ,nqlat  ,ngrid  ,lwqin  ,arexop ,&
            &ndim   ,nameel ,quanel ,unitel ,descel ,&
            &.true. ,inidum ,nameac ,lagstm ,nlags  ,&
            &lgrwt  ,errr   )
            if (errr.ne.0) goto 1000
            ncelst = 1
         else
!
!              Continue with existing restart file
!
            ncelst = ncelst+1
         endif
      endif
!
   else
!
!        Write to restart file.
!
      nrerr = eflwrd
      call FLWRST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr ,&
      &ngrid  ,lwqin  ,ncelst ,itim   ,curtim ,nameel ,&
      &h2     ,q2     ,contrl ,conhis ,strhis ,qaggr  ,&
      &qlaggr ,arexop ,arexcn ,lagstm ,nlags  ,buflag ,&
      &lgrwt  ,grhis  ,errr   )
      if (errr.ne.0) goto 1000
      ncelst = ncelst+1
!
   endif
   goto 1010
!
1000 continue
   if (errr.gt.0) then
      if (errr.eq.4) then
!           Size of buffer has been changed
         call error (juer ,&
         &'FLRSTA  Size of discharge buffer changed at restart' ,&
         &efllag ,ker)
      else
!
!           Could not found restart time step.
!
         call error (juer ,'FLRSTA' ,eflrtt ,ker)
      endif
      ker = fatal
   else
!
!        NEFIS error ( <0 )
      ker = fatal
      write (txt,'(i8)') errr
      call error (juer ,'FLRSTA @'//txt//'@' ,nrerr ,ker)
   endif
!
1010 continue
end
