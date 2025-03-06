Program TestGreenAmpt

  use GreenAmptInfiltration

  Double precision   KSat, Psi, Theta_Dmax, Theta_D, Theta_DU
  Double precision   Lu, Kr, Tr
  Double precision   DepthPonding, F, Fprev, T, CumRain, CumInf, InfRate, smallf, Inf_currentstep

  Integer            Idebug, ntim, TimestepSize, itime
  Double precision   Rainfall (120)

  idebug = 41
  Open (Idebug, file ='TestGreenampt.out', status='unknown')


! Input
   Ksat = 0.1 * 25.4d0   ! in mm/hour
   Psi  = 2.0 * 25.4d0   ! in mm
   Theta_DMax = 0.2

! initialisation
   Theta_d  = Theta_DMax
   Theta_du = Theta_DMax

 ! Rainfall, kwartiercijfers in mm
   ntim = 120
   Rainfall(1) =   0.
   Rainfall(2) =   0.4445
   Rainfall(3) =   0.762
   Rainfall(4) =   1.2065
   Rainfall(5) =   1.7145
   Rainfall(6) =   2.159
   Rainfall(7) =   2.6035
   Rainfall(8) =   2.9845
   Rainfall(9) =   3.4925
   Rainfall(10) =  3.937
   Rainfall(11) =  4.0005
   Rainfall(12) =  3.81
   Rainfall(13) =  3.556
   Rainfall(14) =  3.2385
   Rainfall(15) =  2.9845
   Rainfall(16) =  2.667
   Rainfall(17) =  2.413
   Rainfall(18) =  2.0955
   Rainfall(19) =  1.8415
   Rainfall(20) =  1.524
   Rainfall(21) =  1.27
   Rainfall(22) =  1.016
   Rainfall(23) =  0.6985
   Rainfall(24) =  0.4445
   Rainfall(25) =  0.127
   Rainfall(26) =  0.
   Rainfall(27) =  0.
   Rainfall(28) =  0.
   Rainfall(29) =  0.
   Rainfall(30) =  0.
   Rainfall(31) =  0.
   Rainfall(32) =  0.
   Rainfall(33) =  0.
   Rainfall(34) =  0.
   Rainfall(35) =  0.
   Rainfall(36) =  0.
   Rainfall(37) =  0.
   Rainfall(38) =  0.
   Rainfall(39) =  0.
   Rainfall(40) =  0.
   Rainfall(41) =  0.
   Rainfall(42) =  0.
   Rainfall(43) =  0.
   Rainfall(44) =  0.
   Rainfall(45) =  0.
   Rainfall(46) =  0.
   Rainfall(47) =  0.
   Rainfall(48) =  0.
   Rainfall(49) =  0.
   Rainfall(50) =  0.
   Rainfall(51) =  0.
   Rainfall(52) =  0.
   Rainfall(53) =  0.
   Rainfall(54) =  0.
   Rainfall(55) =  0.
   Rainfall(56) =  0.
   Rainfall(57) =  0.
   Rainfall(58) =  0.
   Rainfall(59) =  0.
   Rainfall(60) =  0.
   Rainfall(61) =  0.
   Rainfall(62) =  0.
   Rainfall(63) =  0.
   Rainfall(64) =  0.
   Rainfall(65) =  0.
   Rainfall(66) =  0.
   Rainfall(67) =  0.
   Rainfall(68) =  0.
   Rainfall(69) =  0.
   Rainfall(70) =  0.
   Rainfall(71) =  0.
   Rainfall(72) =  0.
   Rainfall(73) =  0.
   Rainfall(74) =  0.
   Rainfall(75) =  0.
   Rainfall(76) =  0.
   Rainfall(77) =  0.
   Rainfall(78) =  0.
   Rainfall(79) =  0.
   Rainfall(80) =  0.
   Rainfall(81) =  0.
   Rainfall(82) =  0.
   Rainfall(83) =  0.
   Rainfall(84) =  0.
   Rainfall(85) =  0.
   Rainfall(86) =  0.
   Rainfall(87) =  0.
   Rainfall(88) =  0.
   Rainfall(89) =  0.
   Rainfall(90) =  0.
   Rainfall(91) =  0.
   Rainfall(92) =  0.
   Rainfall(93) =  0.
   Rainfall(94) =  0.
   Rainfall(95) =  0.
   Rainfall(96) =  0.
   Rainfall(97) =  0.
   Rainfall(98) =  0.
   Rainfall(99) =  0.
   Rainfall(100) =  0.
   Rainfall(101) =  0.
   Rainfall(102) =  0.
   Rainfall(103) =  0.
   Rainfall(104) =  0.
   Rainfall(105) =  0.
   Rainfall(106) =  0.
   Rainfall(107) =  0.
   Rainfall(108) =  0.
   Rainfall(109) =  0.
   Rainfall(110) =  0.
   Rainfall(111) =  0.
   Rainfall(112) =  0.
   Rainfall(113) =  0.
   Rainfall(114) =  0.
   Rainfall(115) =  0.
   Rainfall(116) =  0.
   Rainfall(117) =  0.
   Rainfall(118) =  0.
   Rainfall(119) =  0.
   Rainfall(120) =  0.

 ! GreenAmpt2onstants
   Call SetGreenAmptConstants (KSat, Psi, Theta_DMax, Lu, Kr, Tr, idebug)

   TimestepSize = 900
   F  = 0.00001d0  !not =0 ivm divide by zero
   T  = 0.d0
   CumRain = 0.0d0
   itime = 0

   Write(Idebug,*) ' itime  RainRate RainStep CumRain InfRate Inf_currentstep CumInf Theta_DU  T'
   Write(Idebug,'(I5,10F10.4)') itime, 0.0, 0.0, CumRain, 0.0, 0.0, CumInf, Theta_DU, T

!  Computation time loop
   Do itime=1,ntim
!     call greenampt formula with proper input (mm, mm/hour, etc)
      DepthPonding = 0.0d0
      Fprev = F  ! previous cum. infiltration
      Call GreenAmpt (Rainfall(itime)*4,DepthPonding, Theta_D, Theta_DU, F, T, &
                      KSat, Psi, Theta_DMax, Lu, Kr, Tr, smallf, &
                      Idebug, TimestepSize)
      CumRain = CumRain + Rainfall(itime)
      CumInf  = F
      Inf_currentStep = max (0.0D0, F - Fprev)
      InfRate = smallf  ! Inf_currentStep * 3600.D0/TimestepSize

      Write(Idebug,'(I5,10F10.4)') itime,Rainfall(itime)*4, Rainfall(itime), CumRain, InfRate, Inf_currentstep, CumInf, Theta_DU, T
   Enddo

end program TestGreenAmpt
