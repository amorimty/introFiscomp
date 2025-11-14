program exerA
    implicit none
    
    real(8) :: r,x, eps, s, x2=0.0_8, lambda, der, d, dsat=0.15_8
    real(8) :: lambdaMinimos
    real(8) :: sum_i=0.0_8, sum_y=0.0_8, sum_ii=0.0_8, sum_iy=0.0_8
    integer(8) :: i = 0, n=1000, ntemp=100, count=0, isSat=0

    ! r = 3.95_8
    ! x = 0.8_8

    read(*,*) x
    read(*,*) r
    read(*,*) eps

    open(unit=1, file='distA_out.dat', status='replace', action='write')


    x2 = x + eps
    s = 0.0_8

    do i = 1, n
        ! Iteracao das orbitas
        x = r*x*(1.0_8 - x)
        x2 = r*x2*(1.0_8 - x2)
        d = abs(x2 - x)

        ! Calculo do expoente de Lyapunov
        if(i>ntemp) then
            der = r*(1.0_8 - 2.0_8*x)

            if(abs(der) < 1.0e-15_8) then
                der = 1.0e-10_8
            end if
            
            s = s + log(abs(der))
        end if

        write(1,*)i, x, d

        if(d > dsat) isSat = 1

        ! Soma do minimos quadrados
        if (d /= 0 .and. isSat == 0 .and. i>5) then
            ! write(1,*)i, log(d)

            sum_i  = sum_i  + real(i,8)
            sum_y  = sum_y  + log(d)
            sum_ii = sum_ii + real(i,8)**2
            sum_iy = sum_iy + real(i,8)*log(d)
            count  = count + 1
        end if

    end do

    lambdaMinimos = (count*sum_iy - sum_i*sum_y) / (count*sum_ii - sum_i*sum_i)

    lambda = s/real(n-ntemp,8)
    write(*,*) 'Expoente de Lyapunov Minimos Quadrados = ', lambdaMinimos
    write(*,*) 'Expoente de Lyapunov Formula = ', lambda

    close(1)  

end program exerA

