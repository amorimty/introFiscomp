program name

    implicit none
    integer, allocatable :: b(:), c(:)
    integer :: n, i, j, count = 0, lim

    read(*,*) n

    if ( n < 2 ) then
        open(1, file='primos_out.dat', status='replace')
        close(1)
        stop 1
    end if

    ! alocando um vetor de uma dimensão e de indices 2 - n
    allocate(b(2:n))

    ! colocando todos os numeros de 2 a n no vetor b
    do i = 2, n, 1
        b(i) = i
    end do
    lim = int(sqrt(real(n)))

    do i = 2, lim

        if (b(i) /= 0) then

            do j = i+1, n, 1
                if (b(j) /= 0 .and. mod(b(j), b(i)) == 0) b(j) = 0
            end do

        end if
        
    end do

    ! incrementando 1 no count porque o ultimo primo que tiver o quadrado maior que n não vai ser contado
    do i = 2, n
        if ( b(i) /= 0 ) count = count + 1
    end do


    ! alocando vetor contendo apenas os numeros com tamanho count
    allocate(c(count))
    j = 1
    do i = 2, n, 1
        if (b(i) /= 0) then
            c(j) = b(i)
            j = j+1
        end if
    end do


    open(1, file='primos_out.dat', status='replace')


    do i = 1, (size(c))
        write(1,*) c(i)
    end do

    close(1)
    deallocate(b, c)

end program name
