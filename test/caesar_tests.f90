module caesar_tests
  use caesar, only: caesar_crypt, caesar_translate

  implicit none

  private
  public :: test_caesar_crypt, test_caesar_translate

contains

  subroutine test_caesar_crypt

    character(:), allocatable :: result,expect, sentence
    expect = "ERQMRXU"
    sentence = "BONJOUR"
    result = caesar_crypt(sentence)

    if (result == expect) then
      print *, 'test passed with no index'
    else
      print *, 'test failed with no index'
    end if

    expect = "DQPLQWT"
    result = caesar_crypt(sentence,2)

    if (result == expect) then
      print *, 'test passed with index'
    else
      print *, 'test failed with index'
    end if

  end subroutine test_caesar_crypt

  subroutine test_caesar_translate

    character(:), allocatable :: result,expect, sentence
    sentence = "ERQMRXU"
    expect = "BONJOUR"
    result = caesar_translate(sentence)

    if (result == expect) then
      print *, 'test translate passed with no index'
    else
      print *, 'test translate failed with no index'
    end if

    sentence = "DQPLQWT"
    result = caesar_translate(sentence,2)

    if (result == expect) then
      print *, 'test translate passed with index'
    else
      print *, 'test translate failed with index'
    end if

  end subroutine test_caesar_translate

end module caesar_tests


program run_tests
  use caesar_tests, only: test_caesar_crypt, test_caesar_translate
  call test_caesar_crypt()
  call test_caesar_translate()
end program run_tests