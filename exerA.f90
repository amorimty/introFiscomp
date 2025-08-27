program exerA
    implicit none
    
    real(4) :: a1 = 1, temp1
    real(8) :: a2 = 1, temp2
    real(16) :: a3 = 1, temp3
    integer :: i, i1, i2, i3


    WRITE(*, *) "PRECISAO SIMPLES"

    do  i = 1, 1000, 1
        temp1 = a1 + 1

        if ( (temp1 - 1) == 0 ) then
            i1 = i
            exit
        else
            WRITE(*, *) a1, temp1
            a1 = a1/2
        end if 
        

    end do

    WRITE(*, *) "PRECISAO DUPLA"

    do i = 1, 1000, 1

        temp2 = a2 + 1

        if ( (temp2 - 1) == 0 ) then
            i2 = i
            exit
        else
            WRITE(*, *) a2, temp2
            a2 = a2/2
        end if 

    end do    

    WRITE(*, *) "PRECISAO QUADRUPLA"

    do i = 1, 1000, 1

        temp3 = a3 + 1

        if ( (temp3 - 1) == 0 ) then
            i3 = i
            exit
        else
            WRITE(*, *) a3, temp3
            a3 = a3/2
        end if 

    end do    

    write(*,*) (i1 - 1)
    write(*,*) (i2 - 1)
    write(*,*) (i3 - 1)

end program exerA
