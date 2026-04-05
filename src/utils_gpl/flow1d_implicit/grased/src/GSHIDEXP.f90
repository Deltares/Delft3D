subroutine gshidexp (initra ,nfrac  ,ngrid  ,grn   ,dfrac  ,&
&relden ,chezy  ,velo   ,p     ,hidexpp,&
&igr    ,hidexpf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSHIDEXP (Grad Sed Transp Form HIDing and EXPosure)
!
! Module description: Calculate the Hiding and exposure factor.
!
!                     [ Laguzzi,Febr 1994,Q1660 / par 6.2.1 ]
!
!-----------------------------------------------------------------------
! Parameters:
! NAME              IO DESCRIPTION
! chezy             I  Chezy value
! dfrac             I  Grain sizes per fraction
! g                 I  Acceleration of gravity.
! grn               I  Contains grain sizes Dm, D50. D35 and D90.
! hidexpf           I  Hiding and exposure factor per fraction
! hidexpp           IO parameters to calculate hiding and exposure factor.
!                      (1) = type of hiding and exposure effect.
!                            0 : no hiding and exposure
!                            1 : Egiazaroff, Ashida & Michiue (standard)
!                            2 : Egiazaroff, Ashida & Michiue (experimental)
!                            3 : Proffitt & Sutherland
!                            4 : Wu , Wang & Jia
!                      For type 1 + 2:
!                      (2) = flag for grainsize (type 2)
!                            0 : Dref = Dmed
!                            1 : Dref = D50
!                      (3) = coeff alpha (0.4 for type 1)
!                      (4) = exponent gamma (2 for type 1)
!                      (5) = User defined grain size; overrules par 2 (type 2)
!                      (8) = log(19)
!                      (9) = ddswitch
!                      (10)= beta
!                      For type 4
!                      (2) = 0 : hiding and exp power = 0.6 by default
!                            1 : hiding and exp power specified by user
!                      (3) = non default hiding and exp power
! hrad              I  hydraulic radius
! initra            I  True for initialization of transport formulas
!                      (calculation of constants) else False.
! nfrac             I  number of fractions
! ngrid             I  number of grid points
! p(ngrid,nfrac)    I  probability of transport layer
! relden            I  relative density
! velo              I  velocity (without sign)
!=======================================================================
!
!     Declaration of parameters
!
   integer    nfrac  ,ngrid ,igr
   real       relden ,chezy ,velo
   real       hidexpp (10)  ,hidexpf(nfrac) ,dfrac(nfrac),&
   &grn    (4)    ,p(ngrid,nfrac)
   logical    initra
!
!     Declaration of local parameters
!
   integer    hidextyp,if      ,jf
   real       dref    ,shields ,da    ,dida   ,phi  ,eps
   integer     d35    ,d50     ,d90   ,dmed
   parameter  (d35=1  ,d50=2   ,d90=3 ,dmed=4)

   hidextyp = nint(hidexpp(1))+1
   if (initra) then
!
!        Calculation of constants in time.
!
      goto (90,10,20,90,40) hidextyp

!        Egiazaroff, Ashida and Michiu hiding and exposure factor(standard)
10    continue
      hidexpp(3) = 0.4
      hidexpp(4) = 2.0
      call egiazarofa (hidexpp)
      goto 90

!        Egiazaroff, Ashida and Michiu hiding and exposure factor(experimental)
20    continue
      call egiazarofa (hidexpp)
      goto 90

!        Wu , Wang and Jia hiding and exposure (set default)
40    continue
      if (nint(hidexpp(2)) .eq. 0) hidexpp(3) = 0.6

90    continue

   else

      goto (100,110,120,130,140) hidextyp

!        no hiding and exposure factor
100   continue
      do jf=1,nfrac
         hidexpf(jf) = 1.0
      enddo
      goto 190

!        Egiazaroff, Ashida and Michiu hiding and exposure factor(standard)
110   continue
      dref = grn(dmed)
      call egiazarofb (hidexpp ,dref ,dfrac, nfrac, hidexpf)
      goto 190

!        Egiazaroff, Ashida and Michiu hiding and exposure factor(experimental)
120   continue
      if (nint(hidexpp(2)).eq.0) then
         dref = grn(dmed)
      else
         dref = grn(d50)
      endif
      if (hidexpp(5) .gt. 0.) dref = hidexpp(5)
      call egiazarofb (hidexpp ,dref ,dfrac, nfrac, hidexpf)
      goto 190

!        Proffitt & Sutherland hiding and exposure factor
130   continue
      shields = (velo/chezy)**2 / (grn(d50) * relden)
      if (shields .le. 0.04) then
         da = grn(d50) * 1.08
      else if (shields .lt. 0.045829) then
         da = grn(d50) * (1.944 -21.6 * shields)
      else if (shields .lt. 0.097019) then
         da = grn(d50) * (1.40 - 9.73 * shields)
      else
         da = grn(d50) * 0.456
      endif
!
      do jf=1,nfrac
         dida = dfrac(jf) / da
         if (dida .le. 0.075) then
            eps = 0.40378
         else if (dida .lt. 3.7) then
            eps = 0.53 * log10(dida) + 1.0
         else
            eps = 1.301146
         endif
         hidexpf(jf) = 1./eps**2
      enddo
      goto 190

!        hiding and exposure according to Wu , Wang and Jia
140   continue
      do if=1,nfrac
         phi = 0.
         do jf=1,nfrac
            phi = phi +&
            &p(igr,jf)*dfrac(jf)/(dfrac(if)+dfrac(jf))
         enddo
         hidexpf(if) = (phi/(1.0-phi))**hidexpp(3)
      enddo
190   continue
   endif
end
!
subroutine egiazarofa (hidexpp)
!
!     Initialize time independent constants in the
!     Egiazaroff, Ashida and Michiu hiding and exposure factor
!
   real       hidexpp (10)

!     log19
   hidexpp(8)  = log10(19.)
!     ddswitch
   hidexpp(9)  = max(hidexpp(3),0.1)
!     beta          ddswitch         log19            ddswitch
   hidexpp(10) = hidexpp(9)*(hidexpp(8)/log10(19.*hidexpp(9)))&
   &**hidexpp(4)
end

subroutine egiazarofb (hidexpp ,dref ,dfrac, nfrac, hidexpf)
!
!     Calculate hiding and exposure factor according to
!     Egiazaroff, Ashida and Michiu
!
   integer    nfrac
   real       dref
   real       hidexpp (10),hidexpf(nfrac) ,dfrac(nfrac)
!
!     local  variables
!
   integer    jf
   real       log19, ddswitch, beta, dd
!
   log19    = hidexpp(8)
   ddswitch = hidexpp(9)
   beta     = hidexpp(10)

   do jf=1,nfrac
      dd = dfrac(jf) / dref
      if (dd .lt. ddswitch   ) then
         hidexpf(jf) = beta / dd
      else
         hidexpf(jf) = (log19 / log10(19. * dd)) ** hidexpp(4)
      endif
   enddo
end
