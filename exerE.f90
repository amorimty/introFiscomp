program exerE
    ! https://ergodic.ugr.es/cphys/lecciones/fortran/power_method.pdf
    implicit none
    real(8) :: epsilon, lambda, normx
    real(8), allocatable :: A(:, :), x(:), y(:), z(:), r(:)
    integer :: dim, i, j

    write(*,*) 'insira a precisao'
    read(*,*) epsilon
    
    write(*,*) 'dimensao'
    read(*,*) dim
    
    if((rows>0) .and. (columns>0)) allocate(A(dim, dim), x(dim), y(dim), z(dim), r(dim))

    call random_number(x)
    
    normx = sqrt(dot_product(x,x))
    ! normalizando x para evitar um vetor com numeros muito pequenos
    x = x/normx

    do i = 1, dim
        write(*,'("Digite os elementos da linha ", I2, ":")') i
        do j = 1, dim
            write(*, '("Elemento coluna", I2, ":")') j
            read(*,*) A(i,j)
        end do
    end do


    do i = 1, 100, 1

        ! Ax
        y = matmul(A, x)
        

        ! calculo do cociente de rayleigh para encontrar o autovalor
        lambda = DOT_PRODUCT(x, y)/ DOT_PRODUCT(x,x)

        ! normalização para proxima iteração
        normx = sqrt(dot_product(y,y))

        if(normx == 0.0_8) then
            write(*,*) "deu ruim na multiplicação de matriz e a matriz y anulou a normalização"
            exit
        end if

        x = y/normx

        ! calculo do residuo = Ax - lambda x
        z = matmul(A, x)

        r = z - lambda * x

        res = sqrt(dot_product(r,r))

        if(res < epsilon) exit

    end do

    

end program exerE
