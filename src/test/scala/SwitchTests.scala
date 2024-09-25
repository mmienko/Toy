import Signal.*

object SwitchTests extends weaver.FunSuite {
  test("`input/off`") {
    expect.same(Switch.`input/off`(Off, Off), Off) and
    expect.same(Switch.`input/off`(Off, On), Off) and
    expect.same(Switch.`input/off`(On, Off), On) and
    expect.same(Switch.`input/off`(On, On), Off)
  }

  test("`on/off`") {
    expect.same(Switch.`on/off`(Off), On)
    expect.same(Switch.`on/off`(On), Off)
  }
}
