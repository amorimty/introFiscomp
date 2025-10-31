


program exercicio
    implicit none

    type coords
        real(8) :: x
        real(8) :: y
        real(8) :: z
        real(8) :: vx
        real(8) :: vy
        real(8) :: vz
    end type coords  

    type (coords) :: trave_e, trave_d, traj

    ! passando a velocidade inicial de km por hora para metros por segundo
    real(8) :: v = (100.0_8/3.6_8), omega = 39 , x_prev, y_prev, z_prev, x, y ,z, frac

    ! parametros gamma
    real(8) :: a1 = 0.0039_8, a2 = 0.0058_8, vd = 35.0_8, delta = 5.0_8

    real(8) :: dt = 0.01_8, g = 9.8_8
    real(8) :: m = 1.0_8, param, theta, phi, gamma

    open(unit=1, file='chute_out.dat', status='replace', action='write')

    ! definindo limites do travessao do gol de 6m
    ! trave_e = cords(40.0_8, 10.0_8, 2.5_8)
    trave_e%x = 40.0_8
    trave_e%y = 10.0_8
    trave_e%z = 2.5_8

    ! trave_d = cords(40.0_8, 4.0_8, 2.5_8)
    trave_d%x = 40.0_8
    trave_d%y = 4.0_8
    trave_d%z = 2.5_8

    ! definindo inicio da trajetoria da bola

    ! theta e phi sao em radianos
    ! read(*,*) param
    ! read(*,*) theta
    ! read(*,*) phi

    param = 0.001
    theta = 0.480
    phi = 0.1745

    traj%x = 0
    traj%y = 0
    traj%z = 0
    x_prev= traj%x
    y_prev= traj%y
    z_prev= traj%z

    traj%vx = v*sin(theta)*cos(phi)
    traj%vy = v*sin(theta)*sin(phi)
    traj%vz = v*cos(theta)

    write(1,'(E26.16,1X,E26.16)') traj%x, traj%y
    
    
    do 
        x_prev= traj%x
        y_prev= traj%y
        z_prev= traj%z
        
        traj%x = traj%x + traj%vx * dt
        traj%y = traj%y + traj%vy * dt
        traj%z = traj%z + traj%vz * dt

        ! calculando modulo da velocidade do passo anterior
        v = sqrt(traj%vx**2 + traj%vy**2 + traj%vz**2)

        ! calculando o gamma baseado no v do passo anterior
        gamma = a1 + (a2/(1 + exp((v - vd)/delta)))

        traj%vx = traj%vx - (gamma*v*traj%vx/m + param*omega*traj%vy/m)*dt
        traj%vy = traj%vy - (gamma*v*traj%vy/m - param*omega*traj%vx/m)*dt
        traj%vz = traj%vz - (g + gamma*v*traj%vz/m)*dt

        ! Barra a execução quando a bola bate no chao e anuncia que nao foi gol
        if ( traj%z < 0.11_8  .and. traj%x < 40.0_8) then
            write(*,*) 'nao'
            exit
        end if   

        ! Se o x >= 40m ele para no if, interpola e computa o ultimo valor para a bola em x=40
        if ( traj%x >= 40.0_8 ) then
            ! intepolacao para mostrar o ponto onde y é 0
            if (abs(traj%x - x_prev) > 1.0e-12_8) then
                frac = (40.0_8 - x_prev) / (traj%x - x_prev)
            else
                frac = 0.0_8
            end if

            y = y_prev + frac*(traj%y - y_prev)
            z = z_prev + frac*(traj%z - z_prev) 
            x = 40.0_8

            write(1,'(E26.16,1X,E26.16)') x, y

            ! TESTAR SE A BOLA BATE NO TRAVESSAO LEVANDO EM CONSIDERACAO O RAIO DA BOLA DE 11 CM
            ! OU SE PASSA ENTRE AS TRAVES

            if ( y >= (trave_d%y + 0.11_8) .and. y <= (trave_e%y - 0.11_8) .and. z <= (trave_e%z - 0.11_8) .and. z >= (0.11_8) ) then
                write(*,*) 'sim'
            else
                write(*,*) 'nao'
            end if
            exit
        end if

        ! write(1,'(E26.16,1X,E26.16,1X,E26.16)') traj%x, traj%y, traj%z
        write(1,'(E26.16,1X,E26.16)') traj%x, traj%y




    end do

    close(1)
    
end program exercicio

