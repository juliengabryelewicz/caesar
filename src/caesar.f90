module caesar
  implicit none
  public :: caesar_translate
contains
  function caesar_translate(sentence,index) result(translated)
    character(:), allocatable :: sentence, translated
    integer, optional :: index
    integer :: index2
    character :: ch
    integer :: i,j
    sentence = trim(sentence)
    translated = ""

    if (present(index)) then
      if (index<26 .and. index>0) then
        index2 = index 
      else
        print *, 'Erreur : index doit être entre 1 et 25 inclus'
      end if
    else
      index2 = 3
    end if

    do i=0,len(sentence)
      do j=65,90
          ch=achar(j)
          if (sentence(i+1:i+1) == ch) then
            if (j>=90-index2) then
              translated = translated // achar(j-26+index2)
            else
              translated = translated // achar(j+index2)
            end if
          end if
      end do
    end do

  end function caesar_translate
end module caesar
