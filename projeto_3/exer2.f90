program exer2

    implicit none
    real(16) :: angulo, v0, vx, vy, delta_t, g, x, y
    real(16) :: x_prev, y_prev, vy_prev, frac
    integer :: i

    v0 = 500.0_16
    g = 9.8_16
    delta_t = 0.1_16
    x = 0.0_16
    y = 0.0_16
    i = 0

    read(*,*) angulo
    vx = v0*cosd(angulo)
    vy = v0*sind(angulo)
    
    open(1, file='output2.txt', status='replace')

    write(1,*) x, y  


    do
        x_prev = x
        y_prev = y
        vy_prev = vy

        ! passo de Euler
        x = x + vx*delta_t
        y = y + vy*delta_t
        vy = vy - g*delta_t

        if (y < 0.0_16) exit   ! saiu do chão
        write(1,*) x, y
    end do

    frac = y_prev / (y_prev - y)
    x = x_prev + frac*(x - x_prev)
    y = 0.0_16
    write(1,*) x, y

    close(1)

end program exer2
