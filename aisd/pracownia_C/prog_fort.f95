character(len=20) function str(k)
    integer, intent(in) :: k
    write (str, *) k
    str = adjustl(str)
end function str

program zadaniec
    implicit none
    integer :: k, l, i, j, s, vmin, imin, vcur
    integer, dimension(:), allocatable :: a, b, reps, tmp
    integer, dimension(:, :), allocatable :: t, cut
    character(20) :: str

    read *, k, l

    allocate (tmp(l))
    read (*,*) tmp

    allocate (reps(l+1))
    reps(l+1) = 0
    do i=1, l
        reps(i) = tmp(i)
    end do
    deallocate (tmp)

    allocate (a(l+1))
    a(l+1) = 0
    do i=0, l-1
        a(l-i) = a(l-i+1) + reps(l-i)
    end do


    allocate (b(l+1))
    b(l+1) = 0
    do i=0, l-1
        b(l-i) = a(l-i) + b(l-i+1)
    end do

    allocate (cut(l+1, k))

    allocate (t(l+1, k))
    t(l+1, 1) = 0
    do i=0, l-1
        t(l-i,1) = b(l-i)
        cut(i+1,1) = l
    end do

    do j=2, k
        do i=1, l
            vmin = t(i+1, j-1) + reps(i)
            imin = i+1

            do s = i+2, cut(i,j-1)
                vcur = t(s, j-1) + b(i) - b(s) - (s-i)*a(s)
                if (vcur < vmin) then
                    vmin = vcur
                    imin = s
                end if
            end do

            t(i,j) = vmin
            cut(i,j) = imin
        end do

        t(l+1,j) = 0
    end do

    write(*,"(A)") str(t(1,k))

    j = 1
    s = 1
    do i = 0, k-2
        s = j
        j = cut(j, k-i)
        write(*,"(A,$)") str(j - s)
    end do
    write(*,"(A)") str(l - j + 1)

    call EXIT(0)
end
