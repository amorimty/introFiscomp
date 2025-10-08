program exer2

    implicit none
    real(16) :: angulo = 0, v0 = 500.0_16, vx, vy_curr, delta_t = 0.1_16, g = 9.81_16, x=0, y=0
    integer :: i = 0

    read(*,*) angulo
    
    vx = v0*cosd(angulo)
    vy_curr = v0*sind(angulo)
    
    open(1, file='output2.txt', status='replace')
    write(1, *)  x, y

    do while(y >= 0)

        x = x + vx*delta_t
        y = y + vy_curr*delta_t
        vy_curr = vy_curr -g*delta_t

        write(1,*) x, y
    end do

    close(1)

end program exer2
