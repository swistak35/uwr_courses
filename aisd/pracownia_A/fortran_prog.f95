character(len=20) function str(k)
    !   "Convert an integer to string."
        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)
    end function str


program summation
    implicit none
    integer,parameter :: int32 = selected_int_kind(16)
    integer(kind=int32) :: total, mx, half, a, b, dist, n
    integer(kind=int32), dimension(:), allocatable :: tab


    total = 0
    mx = 0
    a = 1
    b = 1
    dist = 0

    read*,n

    allocate (tab(n))
    read (*,*) tab

    total = sum(tab)
    half = total / 2

    !print*, 'N = ', n
    !print*, 'Total sum = ',total
    !print*, 'Half = ', half

    do
        if (b > n) exit

        if (dist <= half) then
            dist = dist + tab(b)
            b = b + 1
            mx = max(mx, min(dist, total-dist))
        end if

        if (dist > half) then
            dist = dist - tab(a)
            a = a + 1
            mx = max(mx, min(dist, total-dist))
        end if
    end do

    write(*,"(a)") str(mx)
    print*, mx

    close(10)
end
