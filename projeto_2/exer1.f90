program exer1

    implicit none
    real(16) :: resultados(7), d1, d2, d3, x0
    real(16), allocatable :: h(:)
    integer :: n, i

    x0 = 0.5_16
    d1 = 2.0_16*x0 / (1.0_16 + x0*x0)
    d2 = 2.0_16*(1.0_16 - x0*x0) / (1.0_16 + x0*x0)**2
    d3 = 4.0_16*x0*(x0**2 - 3.0_16) / (1.0_16 + x0*x0)**3

    open(1, file='tab1_in.dat', status='old')
    open(2, file='tab1_out.dat', status='replace')
    
    
    
    read(1,*) n
    write(*,*) n

    allocate(h(n))

    read(1,*) h


    do i = 1, n, 1
        resultados(1) = h(i)
        resultados(2) = abs( derivadaSimetrica3 (x0, h(i)) - d1 )  
        resultados(3) = abs( derivadaFrente2    (x0, h(i)) - d1 )  
        resultados(4) = abs( derivadaTras2      (x0, h(i)) - d1 )  
        resultados(5) = abs( derivadaSegunda3   (x0, h(i)) - d2 )  
        resultados(6) = abs( derivadaSegunda5   (x0, h(i)) - d2 )  
        resultados(7) = abs( derivadaTerceira5  (x0, h(i)) - d3 )  
        write(2,*) resultados
    end do

    close(1)
    close(2)

contains

    real(16) function f(x)
        real(16), intent(in) :: x
        f = log( 1.0_16 + x*x )
    end function f


    real(16) function derivadaFrente2(x0,h)
        real(16), intent(in) :: x0,h
        derivadaFrente2 = ( f(x0+h) - f(x0) ) / h
    end function derivadaFrente2

    real(16) function derivadaTras2(x0,h)
        real(16), intent(in) :: x0,h
        derivadaTras2 = ( f(x0) - f(x0-h) ) / h
    end function derivadaTras2


    real(16) function derivadaSimetrica3(x0,h)
        real(16), intent(in) :: x0,h
        derivadaSimetrica3 = ( f(x0+h) - f(x0-h) ) / (2.0_16*h)
    end function derivadaSimetrica3

    real(16) function derivadaSegunda3(x0,h)
        real(16), intent(in) :: x0,h
        derivadaSegunda3 = ( f(x0+h) - 2.0_16*f(x0) + f(x0-h) ) / (h*h)
    end function derivadaSegunda3


    real(16) function derivadaSegunda5(x0,h)
        real(16), intent(in) :: x0,h
        derivadaSegunda5 = ( -f(x0+2*h) + 16.0_16*f(x0+h) - 30.0_16*f(x0) + 16.0_16*f(x0-h) - f(x0-2*h) ) &
                        / (12.0_16*h*h)
    end function derivadaSegunda5

    real(16) function derivadaTerceira5(x0,h)
        real(16), intent(in) :: x0,h
        derivadaTerceira5 = ( -f(x0-2*h) + 2.0_16*f(x0-h) - 2.0_16*f(x0+h) + f(x0+2*h) ) / (2.0_16*h**3)
    end function derivadaTerceira5





end program exer1
