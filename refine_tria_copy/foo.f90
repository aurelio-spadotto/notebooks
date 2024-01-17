program test
        integer, pointer :: ptr1=>null()
        integer, pointer :: ptr2
        integer , target :: a
        a = 1
        allocate(ptr1)
        ptr1 = 3
        ptr2 =>a
        ptr1 = ptr2 
        print *,ptr2 
end program test
