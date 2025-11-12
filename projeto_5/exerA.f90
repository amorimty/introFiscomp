program exerA
    implicit none
    
    real(8) :: r,x, x_prev, temp = 0.0_8
    integer :: i = 0, n = 1000

    ! r = 3.2_8
    ! x = 0.8_8

    read(*,*) r
    read(*,*) x

    open(unit=1, file='map_linha.dat', status='replace', action='write')
    open(unit=2, file='map_pontos.dat', status='replace', action='write')

    
    write(1, *) x, 0.0_8

    do i = 1, n, 1
        x_prev = x
        x = r*x*(1.0_8 - x)
        ! write(1,*) i, x
        write(2,*) x_prev, x
        write(1,*) x_prev, x
        write(1, *) x, x
        
        if (i == n) print*, 'x* ~', x
    end do

    close(1)  
    close(2)

end program exerA

