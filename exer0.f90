program exer0
    implicit none

    real(4), dimension(3) :: c1, c2, c3, c4, v1, v2, v3
    real(4) :: volume, totalArea, a1, a2, a3, a4
    
    real(4), dimension(4) :: area_triangulo, volume_tetraedro
    integer :: i
    
    read(*, *) c1
    read(*, *) c2
    read(*, *) c3
    read(*, *) c4

    v1 = c2 - c1
    v2 = c3 - c1
    v3 = c4 - c1

    area_triangulo = triangArea(v1, v2)

    volume_tetraedro = volTetraedro(v1, v2, v3)

    
    open(1, file='tetra_out.dat', status='replace')
    write(1, *) volume
    write(1, *) soma_areas
    
    call subroutine(areas)
    
    write(1,*) areas(1)
    do i = 2, 4
        if (areas(i) /= areas(i-1)) then
            write(1,*) areas(i)
        end if
    end do
    
    close(1)
end program exer0

real function triangArea(v1, v2)
    implicit none
    real(4), dimension(3) :: v1,v2,prod_vet
    real(4)::norma

    !calcular produto vetorial

    
    triangArea = (0.5)*norma
        
end function triangArea

real function volTetraedro(v1,v2,v3)
    implicit none
    real(4), dimension(3) :: v1, v2, v3, vec

    real(4)::prodMisto

    vec(1) = (v2(3)*v1(2) - v1(3)*v2(2))
    vec(2) = -(v1(1)*v2(3) - v2(1)*v1(3))
    vec(3) = (v1(1)*v2(2) - v2(1)*v1(2))
    
    DOT_PRODUCT(v1, vec)
    !calcular produto misto

    volume = (1.0/6.0) * abs(prodMisto)
end function volTetraedro

subroutine ordena(lista)
    implicit none
    real(4) :: aux
    real (4), dimension(4) :: lista
    integer :: i, j
    
    do i = 1, 4

        j = i
        aux = lista(j)

        do while ((j>1) .and. (lista(j-1)>aux))
            lista(j) = lista(j-1)
                j = j-1
        end do
        lista(j) = aux
    end do
end subroutine ordena