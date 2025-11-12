program exerB
    implicit none
    
    real(8) :: r,x, eps, s, x2=0.0_8, lambda, der
    integer(8) :: i = 0, n=1000, ntemp=100

    ! r = 3.95_8
    ! x = 0.8_8

    read(*,*) x
    read(*,*) r
    read(*,*) eps

    open(unit=1, file='distA_out.dat', status='replace', action='write')

    x2 = x + eps
    s = 0.0_8

    do i = 1, n
        x = r*x*(1.0_8 - x)
        x2 = r*x2*(1.0_8 - x2)

        if(i>ntemp) then
            der = r*(1.0_8 - 2.0_8*x)

            if(abs(der) < 1.0e-15_8) then
                der = 1.0e-10_8
            end if
            
            s = s + log(abs(der))
        end if
        write(1,*)i, x, abs(x-x2)
        ! write(1,*)i, abs(x-x2)
        
    end do

    lambda = s/real(n-ntemp,8)
    write(*,*) 'Lyapunov Exponent = ', lambda

    close(1)  

end program exerB

