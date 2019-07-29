program myprog
  use foo, only : add

  implicit none
  
  integer :: five

  five = add(2,3)

  if (five .eq. 5) then
  endif
end program myprog