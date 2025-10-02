program exer3

    implicit none
    real(16) :: resultados(10)
    real(16), ALLOCATABLE :: r1(:), r2(:), r3(:), a1(:), a2(:), a3(:), b1(:), b2(:), b3(:)
    integer :: i, k
    open(1, file='tab3_out.dat', status='replace')
    
    
    
    read(*,*) k

    allocate(r1(k), r2(k), r3(k), a1(k), a2(k), a3(k), b1(k), b2(k), b3(k))
    
    ! raiz -1
    CALL busca_direta(f, -1.2_16, 0.4_16, k, r1)

    ! raiz 1
    CALL busca_direta(f, 0.8_16, 0.4_16, k, r2)

    ! raiz 3
    CALL busca_direta(f, 2.8_16, 0.4_16, k, r3)

    ! raiz -1
    CALL newton(f, df, -1.2_16, k, a1)

    ! raiz 1
    CALL newton(f, df, 0.8_16, k, a2)
    
    ! raiz 3
    CALL newton(f, df, 2.8_16, k, a3)


    ! raiz -1
    CALL secante(f, -1.2_16, -0.8_16, k, b1)

    ! raiz 1
    CALL secante(f, 0.8_16, 1.2_16, k, b2)
    
    ! raiz 3
    CALL secante(f, 2.8_16, 3.2_16, k, b3)


    write(1,'(A)') "iter               dir1            dir2            dir3            NR1             NR2             NR3             sec1            sec2            sec3"

    do i = 1, k
    write(1,'(I4,3X,9(ES20.10))') i, r1(i), r2(i), r3(i), a1(i), a2(i), a3(i), b1(i), b2(i), b3(i)
    end do

    close(1)

contains 

    real(16) function f(x)
        implicit none

        real(16), intent(in) :: x
        f = x**3 - 3.0_16*x**2 - x + 3.0_16
    end function f

    real(16) function df(x)
        implicit none   
        real(16), intent(in) :: x
        df = 3.0_16*x**2 - 6.0_16*x - 1.0_16
    end function df

    subroutine newton(func, dfunc, x0, n, raiz)
        implicit none
        real(16), external :: func, dfunc       ! funções passadas como argumento
        real(16), intent(in) :: x0
        integer, intent(in) :: n
        real(16), intent(out) :: raiz(n)
        real(16) :: x
        integer :: i

        x = x0
        do i = 1, n
            x = x - func(x)/dfunc(x)
            raiz(i) = x
        end do

    end subroutine newton

   
    subroutine secante(func, x0, x1, k, raiz)
        implicit none
        real(16), external :: func
        real(16), intent(in) :: x0, x1
        integer,  intent(in) :: k
        real(16), intent(out) :: raiz(k)
        real(16) :: x_prev, x_curr, x_next, denom
        integer :: j

        x_prev = x0
        x_curr = x1

        do j = 1, k
            denom = func(x_curr) - func(x_prev)
            if (denom == 0.0_16) then
                ! evita divisão por zero
                raiz(j) = x_curr
                exit
            end if

            x_next = x_curr - func(x_curr) * (x_curr - x_prev) / denom
            x_prev = x_curr
            x_curr = x_next
            raiz(j) = x_curr
        end do
    end subroutine secante


    subroutine busca_direta(func, x0, h0, num_iter, raiz)
        implicit none
        real(16), external :: func
        integer,  intent(in)  :: num_iter
        real(16), intent(in)  :: x0, h0
        real(16), intent(out) :: raiz(num_iter)

        real(16) :: x_prev, x_curr, x_mid
        real(16) :: f_prev, f_curr, f_plus, f_minus, h
        integer :: j, dir

        h = abs(h0)

        f_plus  = func(x0 + h)
        f_minus = func(x0 - h)
        if (abs(f_plus) <= abs(f_minus)) then
            dir = +1
        else
            dir = -1
        end if

        x_prev = x0
        x_curr = x0 + dir*h
        f_prev = func(x_prev)
        f_curr = func(x_curr)

        do j = 1, num_iter
            if (f_prev * f_curr <= 0.0_16) then
                x_mid = 0.5_16 * (x_prev + x_curr)
                raiz(j) = x_mid

                if ( f_prev * func(x_mid) <= 0.0_16 ) then
                x_curr = x_mid
                f_curr = func(x_curr)
                else
                x_prev = x_mid
                f_prev = func(x_prev)
                end if

                h = 0.5_16 * abs(x_curr - x_prev)
            else
                x_prev = x_curr
                f_prev = f_curr
                x_curr = x_curr + dir*h
                f_curr = func(x_curr)
                raiz(j) = x_curr
            end if
        end do
    end subroutine busca_direta



end program exer3
