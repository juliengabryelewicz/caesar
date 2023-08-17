module caesar_tests
  use caesar, only: caesar_translate

  implicit none

  private
  public :: test_caesar

contains

  subroutine test_caesar

    character(:), allocatable :: result,expect, sentence
    expect = "ERQMRXU"
    sentence = "BONJOUR"
    result = caesar_translate(sentence)

    if (result == expect) then
      print *, 'test passed with no index'
    else
      print *, 'test failed with no index'
    end if

    expect = "DQPLQWT"
    result = caesar_translate(sentence,2)

    if (result == expect) then
      print *, 'test passed with index'
    else
      print *, 'test failed with index'
    end if

  end subroutine test_caesar

end module caesar_tests


program run_tests
  use caesar_tests, only: test_caesar
  call test_caesar()
end program run_tests