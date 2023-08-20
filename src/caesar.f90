module caesar
  implicit none
  public :: caesar_crypt
contains

  function get_index(index) result(new_index)
    integer, optional :: index
    integer :: new_index

    if (present(index)) then
      if (index<26 .and. index>0) then
        new_index = index 
      else
        print *, 'Erreur : index doit être entre 1 et 25 inclus'
      end if
    else
      new_index = 3
    end if    

  end function get_index

  function caesar_crypt(sentence,index) result(translated)
    character(:), allocatable :: sentence, translated
    integer, optional :: index
    integer :: index2
    character :: ch
    integer :: i,j
    sentence = trim(sentence)
    translated = ""

    index2 = get_index(index)

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

  end function caesar_crypt

  function caesar_translate(sentence,index) result(translated)
    character(:), allocatable :: sentence, translated
    integer, optional :: index
    integer :: index2
    character :: ch
    integer :: i,j
    sentence = trim(sentence)
    translated = ""

    index2 = get_index(index)
    
    do i=0,len(sentence)
      do j=65,90
          ch=achar(j)
          if (sentence(i+1:i+1) == ch) then
            if (j<=65+index2) then
              translated = translated // achar(j+26-index2)
            else
              translated = translated // achar(j-index2)
            end if
          end if
      end do
    end do

  end function caesar_translate
end module caesar
