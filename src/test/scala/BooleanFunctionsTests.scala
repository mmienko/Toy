import BooleanFunctions.{Maj, Odd}

object BooleanFunctionsTests extends weaver.FunSuite{

  test("Maj") {
    expect.eql(Maj(0,0,0), 0) and
    expect.eql(Maj(0,0,1), 0) and
    expect.eql(Maj(0,1,0), 0) and
    expect.eql(Maj(0,1,1), 1) and
    expect.eql(Maj(1,0,0), 0) and
    expect.eql(Maj(1,0,1), 1) and
    expect.eql(Maj(1,1,0), 1) and
    expect.eql(Maj(1,1,1), 1)
  }

  test("Odd") {
    expect.eql(Odd(0,0,0), 0) and
    expect.eql(Odd(0,0,1), 1) and
    expect.eql(Odd(0,1,0), 1) and
    expect.eql(Odd(0,1,1), 0) and
    expect.eql(Odd(1,0,0), 1) and
    expect.eql(Odd(1,0,1), 0) and
    expect.eql(Odd(1,1,0), 0) and
    expect.eql(Odd(1,1,1), 1)
  }
}
