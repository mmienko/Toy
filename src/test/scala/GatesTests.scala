import Gates.*

object GatesTests extends weaver.FunSuite {

  test("Not") {
    expect.eql(Not(0), 1) and
      expect.eql(Not(1), 0)
  }

  test("Nor") {
    expect.eql(Nor(0, 0), 1) and
      expect.eql(Nor(0, 1), 0) and
      expect.eql(Nor(1, 0), 0) and
      expect.eql(Nor(1, 1), 0) and
      expect.eql(Gates.Nor(1, 1, 1, 1), 0) and
      expect.eql(Gates.Nor(1, 0, 1, 0), 0) and
      expect.eql(Gates.Nor(1, 0, 0, 0), 0) and
      expect.eql(Gates.Nor(0, 0, 0, 0), 1)
  }

  test("Or") {
    expect.eql(Or(0, 0), 0) and
      expect.eql(Or(0, 1), 1) and
      expect.eql(Or(1, 0), 1) and
      expect.eql(Or(1, 1), 1) and
      expect.eql(Or(1, 1, 1, 1), 1) and
      expect.eql(Or(1, 0, 1, 0), 1) and
      expect.eql(Or(1, 0, 0, 0), 1) and
      expect.eql(Or(0, 0, 0, 0), 0)
  }

  test("And") {
    expect.eql(And(0, 0), 0) and
      expect.eql(And(0, 1), 0) and
      expect.eql(And(1, 0), 0) and
      expect.eql(And(1, 1), 1) and
      expect.eql(And(1, 1, 1, 1), 1) and
      expect.eql(And(1, 0, 1, 0), 0) and
      expect.eql(And(1, 0, 0, 0), 0) and
      expect.eql(And(0, 0, 0, 0), 0)
  }

   test("Xor") {
    expect.eql(Xor(0, 0), 0) and
      expect.eql(Xor(0, 1), 1) and
      expect.eql(Xor(1, 0), 1) and
      expect.eql(Xor(1, 1), 0)
  }
}
