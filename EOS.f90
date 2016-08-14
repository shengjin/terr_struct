SUBROUTINE EOS(press,mass,rho)
!read in initial conditions

USE FRACT
USE PARAEOS

IMPLICIT NONE
INCLUDE 'units.h90'

DOUBLE PRECISION, INTENT(IN)  ::press,mass
DOUBLE PRECISION, INTENT(OUT) ::rho

DOUBLE PRECISION              ::eta

INTEGER                       ::iLayer  ! Layer
! 3 = Core, 2 = Mantle, 1 = Water

LOGICAL,PARAMETER             ::DEBUG=.TRUE.



IF( (mass/Mtot) > (1d0 - frac_water) )THEN
    iLayer=1
ELSE IF( (mass/Mtot) > frac_core )THEN
    iLayer=2
ELSE
    iLayer=3 
END IF


IF( iLayer .eq. 1 )THEN
    CALL RTSEC(K0_water,K0d_water,press,eta,iEOS_water)
    rho=rho0_water*eta
ELSE IF( iLayer .eq. 2 )THEN
    CALL RTSEC(K0_mantle,K0d_mantle,press,eta,iEOS_mantle)
    rho=rho0_mantle*eta
ELSE IF( iLayer .eq. 3 )THEN
    CALL RTSEC(K0_core,K0d_core,press,eta,iEOS_core)
    rho=rho0_core*eta
ELSE
    WRITE(*,*)'SBR EOS.f90: Wrong integer iLayer!'
    stop
END IF


RETURN

END SUBROUTINE EOS

    


subroutine RTSEC(K0,K0d,press,eta,iEOS)
  IMPLICIT NONE
  INTEGER :: ISTEP, iEOS
  REAL*8 :: x1, x2, xacc, DX, Func1, Func2, temp
  REAL*8 :: eta, K0d, K0, press
  xacc = 1.0E-06
  x1  = 0.0
  x2  = 2.0E6
  DX = x1 - x2
  ISTEP = 0
  DO WHILE (ABS(DX).GT.xacc)
    eta = (x1+x2)/2.0
    call FuncEOS(x1,K0,K0d,press,temp,iEOS)
    Func1=temp
    call FuncEOS(eta,K0,K0d,press,temp,iEOS)
    Func2=temp
    IF ((Func1*Func2).LT.0) THEN
      x2  = eta
      DX = x2-x1
    ELSE
      x1  = eta
      DX = x2-x1
    END IF
    ISTEP = ISTEP+1
  END DO
end subroutine RTSEC



subroutine FuncEOS(x,K0,K0d,press,temp,iEOS)
IMPLICIT NONE
  REAL*8 :: temp,x
  REAL*8 :: K0d, K0, press
  INTEGER :: iEOS
  if (iEOS==1) then
          temp=K0*1.5*(x**(7.0/3.0)-x**(5.0/3.0))*(1.0+0.75*(K0d-4.0)*(x**(2.0/3.0)-1.0))-press
  else if (iEOS==2) then
          temp=K0*1.5*(x**(7.0/3.0)-x**(5.0/3.0))*(1.0+0.75*(K0d-4.0)*(x**(2.0/3.0)-1.0))-press
  else
          write(*,*)'SBR EOS: Wrong integer iEOS!'
          stop
  end if
end subroutine FuncEOS
