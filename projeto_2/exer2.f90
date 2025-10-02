program exer2

    implicit none
    
    real(16) :: resultados(5), h, integral, best_val(6)
    integer(4), allocatable :: N(:)
    integer :: k, i, best_N(6), m, idx

    best_val = huge(0.0_16)

    open(1, file='tab2_in.dat', status='old')
    open(2, file='tab2_out.dat', status='replace')
    
    
    
    read(1,*) k

    allocate(N(k))

    read(1,*) N
    
    integral = fIntegral(1.0_16) - fIntegral(0.0_16)

    write(2,'(A)') '   N          h               Trapezio                  Simpson              Bode'

    do i = 1, k, 1
        h = 1.0_16/N(i)

        resultados(1) = N(i)
        resultados(2) = h
        resultados(3) = abs(trapezio(0.0_16,1.0_16,N(i)) - integral)
        resultados(4) = abs(simpson(0.0_16,1.0_16,N(i)) - integral)
        resultados(5) = abs(bode(0.0_16,1.0_16,N(i)) - integral)


        do m = 3, 5
            idx = m - 2
            if (resultados(m) < best_val(idx)) then
                best_val(idx) = resultados(m)
                best_N  (idx) = N(i)
            end if
        end do
        ! write(2,*) resultados
        write(2,'(I6,1X,ES14.6,3(1X,ES20.12))') N(i), h, resultados(3), resultados(4), resultados(5)
    end do

    write(*,*) 'Melhor N por coluna de acordo com menor diferença absoluta:'
    write(*,'(A,I0)') '  Trapézio: ', best_N(1)
    write(*,'(A,I0)') '  Simpson : ', best_N(2)
    write(*,'(A,I0)') '  Bode    : ', best_N(3)

    close(1)
    close(2)

contains

    real(16) function f(x)
        real(16), intent(in) :: x
        f = x*x*cos(x)
    end function f

    real(16) function fIntegral(x)
        real(16), intent(in) :: x
        fIntegral = (x*x - 2)*sin(x) + 2*x*cos(x)
    end function fIntegral


    real(16) function trapezio(a, b, N)
        real(16), intent(in) :: a,b
        integer , intent(in) :: N
        integer :: j
        real(16) :: h, x0

        trapezio = 0.0_16
        h = (b - a) / real(N,kind=16)

        do j = 0, N/2 - 1
            x0 = a + (2*j + 1)*h
            trapezio = trapezio + 0.5_16*h * ( f(x0 + h) + 2.0_16*f(x0) + f(x0 - h) )
        end do
    end function trapezio


    real(16) function simpson(a,b,N)
        real(16), intent(in) :: a,b
        integer , intent(in) :: N
        integer :: j
        real(16) :: h, x0

        simpson = 0.0_16
        h = (b - a) / real(N,kind=16)

        do j = 0, N/2 - 1
        x0 = a + (2*j + 1)*h
        simpson = simpson + (h/3.0_16) * ( f(x0 + h) + 4.0_16*f(x0) + f(x0 - h) )
        end do
    end function simpson


    real(16) function bode(a,b,N)
        real(16), intent(in) :: a,b
        integer , intent(in) :: N
        integer :: j
        real(16) :: h, x0
        bode = 0.0_16
        h = (b - a) / real(N,kind=16)
        if (mod(N,4) /= 0) stop 'N deve ser múltiplo de 4 para Bode 4h'
        do j = 0, N/4 - 1
        x0 = a + 4*j*h
        bode = bode + (2.0_16*h/45.0_16) * &
                ( 7.0_16*f(x0) + 32.0_16*f(x0 + h) + 12.0_16*f(x0 + 2*h) + 32.0_16*f(x0 + 3*h) + 7.0_16*f(x0 + 4*h) )
        end do
    end function bode

end program exer2
