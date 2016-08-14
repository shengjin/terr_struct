******************************************************************************

      subroutine derivee_core(ray,fonct,deriv,n_frontieres)
c
C entree : ray,fonct
C sortie : deriv

      USE PARAMETERS,ONLY:NMAX,nfmax 
      
      implicit none
      double precision,parameter   ::G=6.67D-08 
      double precision,parameter   ::pi= 3.141592654D0
      DOUBLE PRECISION,PARAMETER   ::km=1D5,Rearth=6.371D3*km,Lsun=3.827D33,LJ=8.67D-10*Lsun
      double precision,parameter   ::Mearth=5.97424D27
      double precision,parameter   ::pi4=12.56637061D0


        real*8 mass
        real*8 fonct(NMAX),deriv(NMAX)
        integer i
        real*8 ray,press
        real*8 rho_local,gradient
        integer n_frontieres

        real*8 ray2


        LOGICAL       DEBUG
        PARAMETER     (DEBUG=.FALSE.)

        !WARNing for negative Pressure
        LOGICAL       WARNING
        PARAMETER     (WARNING=.FALSE.)
        !PARAMETER     (DEBUG=.TRUE.)


C        LCONST=.TRUE.
                                       
Cwinisdo 05/02/03
        do i=1,NMAX
           deriv(i)=0.d0
        enddo
Cwinisdo 05/02/03


        press      =   fonct(1)
        mass       =   fonct(2)



C    Density, EOS
        rho_local  = 0d0
        CALL EOS(press,mass,rho_local)


        IF(DEBUG)THEN
           WRITE(*,*)
           WRITE(*,*)'SBR DERIVEE'
           WRITE(*,*)'n_frontieres',nfmax,n_frontieres
           WRITE(*,*)'mass/Mearth',mass/mearth
           WRITE(*,*)'Press',press
           WRITE(*,*)'ray/Rearth',ray/Rearth
        END IF


        IF(Press.LE.0D0)THEN
           IF(WARNING)THEN
               WRITE(*,*)'Negative P in derivee !'
               WRITE(*,*)'ray/Rearth,mass/Mearth,Press',ray/Rearth,mass/mearth,press
           END IF 
           IF(Press<0d0)fonct(1)=1D-10
           IF(Press<0d0)Press=1D-10
        END IF


        ray2 = 1. / (ray*ray)

        deriv(1)   =   -  G * mass * rho_local * ray2
        deriv(2)   =     pi4 / ray2 * rho_local

        return
        end

*******************************************************************************
*******************************************************************************

