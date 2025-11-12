program exerA
    implicit none
    
    real(8) :: r,x, x_prev
    integer(8) :: i = 0, n=1000

    ! r = 3.2_8
    ! x = 0.8_8

    read(*,*) r
    read(*,*) x

    open(unit=1, file='map.dat', status='replace', action='write')

    do i = 1, n, 1
        x_prev = x
        x = r*x*(1.0_8 - x)
        write(1,*) i, x
        ! write(1,*) x_prev, x
        

    end do

    close(1)  

end program exerA

