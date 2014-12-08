import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class Lab5Spec extends FlatSpec {
  
  "mapFirstDoWith" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     def dowith[W]: DoWith[W,List[Int]] = mapFirstWith[W,Int] { 
       (i: Int) => 
         if (i < 0) Some(doreturn(-i)) 
         else None 
     } (l1)
     println("dowith:" + dowith(true))
     assertResult((true,gold1)) { dowith(true) }
     
     assertResult((42,gold1)) { dowith(42) }
  }

  // Probably want to write some tests for castOk, typeInfer, substitute, and step.
}

class TypeInferSpec extends FlatSpec {
  "Null" should "return type of TNull" in {
    val x = Null
    assert(typeInfer(x) === TNull)
  }
  "Number" should "return type of TNumber" in {
    val x = N(1)
    assert(typeInfer(x) === TNumber)
  }
  "String" should "return type of TString" in {
    val x = S("blah")
    assert(typeInfer(x) === TString)
  }
  "Boolean" should "return type of TBool" in {
    val x = B(true)
    assert(typeInfer(x) === TBool)
  }
  "Undefined" should "return type of TUndefined" in {
    val x = Undefined
    assert(typeInfer(x) === TUndefined)
  }
  "Not" should "check for type TBoolean" in {
    // Unary accepts B(true) and B(false)
    assertResult(TBool) {
      typeInfer(Unary(Not, B(true)))
      typeInfer(Unary(Not, B(false)))
    } 
  }
  it should "throw errors when appropriate" in {
    // It does not accept anything else
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Unary(Not, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Unary(Not, S("blahblah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Unary(Not, Undefined)) }
  }

  "Plus" should "check for type TNumber if given two TNumbers" in { 
    assertResult(TNumber) {
      typeInfer(Binary(Plus, N(1), N(2)))
      typeInfer(Binary(Plus, N(0), N(-2)))
    }
  }
  it should "check for type TString if given two TStrings" in {
    assertResult(TString) {
      typeInfer(Binary(Plus, S("blah"), S("blah")))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, B(true), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, B(true), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, B(true), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, N(1), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, N(1), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, N(1), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, S("blah"), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, S("blah"), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, Undefined, B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, Undefined, S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Plus, Undefined, Undefined)) }
  }

  "Minus" should "check for type TNumber if given two TNumbers" in { 
    assertResult(TNumber) {
      typeInfer(Binary(Minus, N(1), N(2)))
      typeInfer(Binary(Minus, N(0), N(-2)))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, B(true), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, B(true), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, B(true), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, N(1), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, N(1), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, N(1), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, S("blah"), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, S("blah"), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, S("blah"), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, Undefined, B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, Undefined, S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Minus, Undefined, Undefined)) }
  }
  "Times" should "check for type TNumber if given two TNumbers" in { 
    assertResult(TNumber) {
      typeInfer(Binary(Times, N(1), N(2)))
      typeInfer(Binary(Times, N(0), N(-2)))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, B(true), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, B(true), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, B(true), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, N(1), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, N(1), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, N(1), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, S("blah"), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, S("blah"), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, S("blah"), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, Undefined, B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, Undefined, S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Times, Undefined, Undefined)) }
  }

  "Div" should "check for type TNumber if given two TNumbers" in { 
    assertResult(TNumber) {
      typeInfer(Binary(Div, N(1), N(2)))
      typeInfer(Binary(Div, N(0), N(-2)))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, B(true), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, B(true), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, B(true), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, N(1), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, N(1), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, N(1), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, S("blah"), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, S("blah"), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, S("blah"), Undefined)) }

    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, Undefined, B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, Undefined, S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Div, Undefined, Undefined)) }
  }
  "Eq" should "check for type TBoolean if types are same, else error" in { 
    assertResult(TBool) {
      typeInfer(Binary(Eq, B(true), B(false)))
      typeInfer(Binary(Eq, N(1), N(2)))
      typeInfer(Binary(Eq, S("blah"), S("blah")))
      typeInfer(Binary(Eq, Undefined, Undefined))
    }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Eq, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Eq, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Eq, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Eq, N(1), B(true))) }
  }
  "Ne" should "check for type TBoolean if types are same, else error" in { 
    assertResult(TBool) {
      typeInfer(Binary(Ne, B(true), B(false)))
      typeInfer(Binary(Ne, N(1), N(2)))
      typeInfer(Binary(Ne, S("blah"), S("blah")))
      typeInfer(Binary(Ne, Undefined, Undefined))
    }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ne, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ne, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ne, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ne, N(1), B(true))) }
  }
  it should "error if either e1 or e2 is a function" in { 
    val x = "x"
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ne, Function(None, Left(List(("x", TString))), None, Var(x)), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ne, B(true), Function(None, Left(List(("x", TString))), None, Var(x)))) }
  }
  "Lt" should "check for type TBoolean if TNumber or TString" in {
    assertResult(TBool) {
      typeInfer(Binary(Lt, N(1), N(1)))
      typeInfer(Binary(Lt, S("blah"), S("blah")))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Lt, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Lt, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Lt, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Lt, N(1), B(true))) }
  }
  "Le" should "check for type TBoolean if TNumber or TString" in {
    assertResult(TBool) {
      typeInfer(Binary(Le, N(1), N(1)))
      typeInfer(Binary(Le, S("blah"), S("blah")))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Le, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Le, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Le, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Le, N(1), B(true))) }
  }
  "Gt" should "check for type TBoolean if TNumber or TString, else error" in {
    assertResult(TBool) {
      typeInfer(Binary(Gt, N(1), N(1)))
      typeInfer(Binary(Gt, S("blah"), S("blah")))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Gt, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Gt, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Gt, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Gt, N(1), B(true))) }
  }

  "Ge" should "check for type TBoolean if TNumber or TString" in {
    assertResult(TBool) {
      typeInfer(Binary(Ge, N(1), N(1)))
      typeInfer(Binary(Ge, S("blah"), S("blah")))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ge, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ge, S("blah"), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ge, Undefined, N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Ge, N(1), B(true))) }
  }
  "And" should "check for type TBoolean" in { 
    assertResult(TBool) {
      typeInfer(Binary(And, B(true), B(false)))
    }
  }
  it should "throw errors when appropriate" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(And, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(And, N(1), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(And, N(1), N(2))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(And, S("blah"), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(And, Undefined, Undefined)) }
  }
  "Or" should "check for type TBoolean" in { 
    assertResult(TBool) {
      typeInfer(Binary(Or, B(true), B(false)))
    }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Or, B(true), N(1))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Or, N(1), B(true))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Or, N(1), N(2))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Or, S("blah"), S("blah"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(Binary(Or, Undefined, Undefined)) }
  }
  "Seq" should "check for same type as e2" in { 
    assertResult(TBool) { typeInfer(Binary(Seq,Undefined, B(true))) }
    assertResult(TString) { typeInfer(Binary(Seq,Undefined, S("blah"))) }
    assertResult(TNumber) { typeInfer(Binary(Seq,Undefined, N(1))) }
    assertResult(TUndefined) { typeInfer(Binary(Seq,Undefined, Undefined)) }
  }
  "If" should "throw error if e1 not of type TBoolean" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(N(1), S("1"), S("2"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(S("blah"), S("1"), S("2"))) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(Undefined, S("1"), S("2"))) }
  }
  it should "check for return type the same as e2 & e3" in { 
    assertResult(TString) { typeInfer(If(B(true), S("1"), S("2")))}
    assertResult(TBool) { typeInfer(If(B(true), B(false), B(true)))}
    assertResult(TNumber) { typeInfer(If(B(true), N(2), N(1)))}
    assertResult(TUndefined) { typeInfer(If(B(true), Undefined, Undefined))}
  }
  it should "throw error if e2 and e3 types differ" in { 
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(B(true), S("1"), B(true)))}
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(B(true), Undefined, B(true)))}
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(B(true), N(1), B(true)))}
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(If(B(true), B(true), Undefined))}
  } 
  
  "Function" should "check for correct return type" in { 
    // Simple example
    assertResult(
      TFunction( Left(List(("param1",TBool),("param2",TBool))),TBool)
      ) {
      typeInfer(
        Function(None, Left(List(("param1",TBool),("param2",TBool))), Some(TBool), B(true))
        )
    }
    // More complex example
    assertResult(
      TFunction( Left(List(("param1",TBool),("param2",TBool))),TNumber)
      ) {
      typeInfer(
        Function(None, Left(List(("param1",TBool),("param2",TBool))), Some(TNumber), Binary(Plus,N(1),N(2)))
        )
    }
  }
  it should "check for correct return type for recursive functions" in {
    assertResult(
      TFunction(Left(List(("x",TNumber))),TNumber)
      ) {
      val f = "f"
      val x = "x"
      val fbody = If(Binary(Eq, Var(x), N(0)), 
        Var(x),
        Binary(Plus, 
          Var(x), 
          Call(
            Var(f), 
            List(Binary(Minus, Var(x), N(1)))
            )
          )
        )
      val e1 = Function(Some(f), Left(List(("x", TNumber))), Some(TNumber), fbody)
      typeInfer(e1)
    }
  }

  it should "throw error if function has wrong return type" in { 
    // Incorrect return type
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(
        Function(None, Left(List(("param1",TBool),("param2",TBool))), Some(TBool), S("blah"))
        )
    }
  }
  it should "support PVar" in {
    assertResult(TFunction(Right((PVar, "x", TBool)), TBool)) {
    	typeInfer(Function(None, Right((PVar, "x", TBool)), Some(TBool), B(true)))
    	typeInfer(Function(None, Right((PVar, "x", TBool)), Some(TBool), Var("x")))
    }
  }
  it should "support PName" in {
    assertResult(TFunction(Right((PName, "x", TBool)), TBool)) {
    	typeInfer(Function(None, Right((PName, "x", TBool)), Some(TBool), B(true)))
    	typeInfer(Function(None, Right((PName, "x", TBool)), Some(TBool), Var("x")))
    }
  }
  it should "support PRef" in {
    assertResult(TFunction(Right((PRef, "x", TBool)), TBool)) {
    	typeInfer(Function(None, Right((PRef, "x", TBool)), Some(TBool), B(true)))
    	typeInfer(Function(None, Right((PRef, "x", TBool)), Some(TBool), Var("x")))
    }
  }
  "Call" should "check return type matches the function's return type" in { 
    assertResult(TBool) {
      typeInfer(Call(
        Function(None, Left(List(("param1",TBool),("param2",TBool))), Some(TBool), Var("param1")),
        List(B(true),B(false))
        ))
    }
    assertResult(TString) {
      typeInfer(Call(
        Function(None, Left(List(("param1",TString),("param2",TBool))), Some(TString), Var("param1")),
        List(S("blah"),B(false))
        ))
    }
    assertResult(TNumber) {
      typeInfer(Call(
        Function(None, Left(List(("param1",TNumber),("param2",TBool))), Some(TNumber), Var("param1")),
        List(N(1),B(false))
        ))
    }
    assertResult(TUndefined) {
      typeInfer(Call(
        Function(None, Left(List(("param1",TUndefined),("param2",TBool))), Some(TUndefined), Var("param1")),
        List(Undefined,B(false))
        ))
    }
  }
  it should "throw error if the call's arg types don't match the function's param types" in { 
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Call(
        Function(None, Left(List(("param1",TString),("param2",TBool))), Some(TBool), Var("param1")),
        List(B(true),B(false))
        ))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Call(
        Function(None, Left(List(("param1",TString),("param2",TNumber))), Some(TBool), Var("param1")),
        List(S("blah"),B(false))
        ))
    }
  }
  it should "throw StaticTypeError if wrong number of args is given" in {
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Call(
        Function(None, Left(List(("param1",TString),("param2",TNumber))), Some(TBool), Var("param1")),
        List(S("blah"),B(false),N(1))
        ))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Call(
        Function(None, Left(List(("param1",TString),("param2",TNumber))), Some(TBool), Var("param1")),
        List(S("blah"))
        ))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Call(
        Function(None, Right((PRef, "param1", TString)), Some(TBool), Var("param1")),
        List(S("blah"), N(1))
        ))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Call(
        Function(None, Right((PRef, "param1", TString)), Some(TBool), Var("param1")),
        List()
        ))
    }
  }
  "Obj" should "check that an obj map returns it's appropriate type map" in { 
    assertResult(TObj(
      Map( ("f1",TBool), ("f2",TNumber), ("f3",TBool), ("f4",TUndefined), ("f5",TNumber) )
      )) {
      typeInfer(Obj(
        Map( ("f1",B(true)), ("f2",N(1)), ("f3",B(false)), ("f4",Undefined), ("f5",N(5)) )
        ))
    }
  }
  "GetField" should "check return type matches type of corresponding field" in { 
    val obj = Obj(Map( ("f1",B(true)), ("f2",N(1)), ("f3",B(false)), ("f4",Undefined), ("f5",N(5)), ("f6",S("blah")) ))
    assertResult(TBool) {
      typeInfer(GetField(obj, "f1"))
      typeInfer(GetField(obj, "f3"))
    }
    assertResult(TNumber) {
      typeInfer(GetField(obj, "f2"))
      typeInfer(GetField(obj, "f5"))
    }
    assertResult(TString) {
      typeInfer(GetField(obj, "f6"))
    }
    assertResult(TUndefined) {
      typeInfer(GetField(obj, "f4"))
    }
  }
  it should "throw error if first parameter is not an Obj" in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(GetField(B(true),"f1")) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(GetField(S("blah"),"f1")) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(GetField(N(1),"f1")) }
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(GetField(Undefined,"f1")) }
  }
  it should "throw error if second parameter is an empty string " in {
    intercept[jsy.lab5.ast.StaticTypeError] { typeInfer(GetField(Obj(Map(("f1",B(true)))),"")) }
  }
  
  
  "Decl" should "infer return type to be same as e2 for MConst" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val retType = typeInfer(Decl(MConst,"x", e1, e2)) 
    assert(retType === TNumber)
  }
  it should "infer return type to be same as e2 for MVar" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val retType = typeInfer(Decl(MVar,"x", e1, e2)) 
    assert(retType === TNumber)
  }
  
  "Assign" should "handle variables correctly" in {
    val expr = Decl(MVar, "x", N(3), Assign(Var("x"), N(5)))
    assert(typeInfer(expr) == TNumber)
  }
  it should "error if assignment is of different type from variable" in {
    intercept[jsy.lab5.ast.StaticTypeError] {
	    val x = Decl(MVar, "x", N(3), B(true))
	    val assignment = Assign(x, S("blah"))
	    typeInfer(assignment)
    }
  }
  it should "handle objects/fields correctly" in {
    val o = Obj(Map(
        ("f1",N(2)),
        ("f2",B(true)),
        ("f3",S("blah")),
        ("f3.5",Undefined),
        ("f4",B(false))))
	val expr = Assign(GetField(o, "f3"), S("newvalue"))
	assert(typeInfer(expr) === TString)
  }
  it should "error if object/field types don't match" in {
    intercept[jsy.lab5.ast.StaticTypeError] {
	    val o = Obj(Map(
	        ("f1",N(2)),
	        ("f2",B(true)),
	        ("f3",S("blah")),
	        ("f3.5",Undefined),
	        ("f4",B(false))))
		val expr = Assign(GetField(o, "f3"), B(true))
		typeInfer(expr)
    }
  }
  
  "Cast" should "check for valid cast type" in {
    // CastOkEq
    assertResult(TNull) { typeInfer(Unary(Cast(TNull),Null)) }
    assertResult(TUndefined) { typeInfer(Unary(Cast(TUndefined),Undefined))}
    assertResult(TNumber) { typeInfer(Unary(Cast(TNumber),N(1)))}
    assertResult(TString) { typeInfer(Unary(Cast(TString),S("blah")))}
    assertResult(TBool) { typeInfer(Unary(Cast(TBool),B(true)))}
    assertResult(TObj(Map())) { 
      typeInfer(Unary(Cast(TObj(Map())),Obj(Map())))
      // CastOkEq
      typeInfer(Unary(Cast(TObj(Map())),Null))
    }
    
    // CastOkObjectUP
    assert(typeInfer(Unary(Cast(
      TObj(Map(
          ("f1", TString),
          ("f2", TNumber),
          ("f3", TBool),
          ("f4", TString),
          ("f5", TNumber),
          ("f6", TBool)
      ))
    ), Obj(Map(
          ("f1", S("blah")),
          ("f2", N(1)),
          ("f3", B(true))
    )))) == 
      TObj(Map(
          ("f1", TString),
          ("f2", TNumber),
          ("f3", TBool),
          ("f4", TString),
          ("f5", TNumber),
          ("f6", TBool)
      )))
    // CastOkObjectDOWN
    assert(typeInfer(Unary(Cast(
      TObj(Map(
          ("f1", TString),
          ("f2", TNumber),
          ("f3", TBool)
      ))
    ), Obj(Map(
          ("f1", S("blah")),
          ("f2", N(1)),
          ("f3", B(true)),
          ("f4", Undefined),
          ("f5", Null)
    )))) == 
      TObj(Map(
          ("f1", TString),
          ("f2", TNumber),
          ("f3", TBool)
      )))
  }
  it should "throw appropriate errors" in {
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Unary(Cast(TNull),Undefined))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Unary(Cast(TUndefined),Null))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Unary(Cast(TString),N(1)))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Unary(Cast(TNumber),S("blah")))
    }
    intercept[jsy.lab5.ast.StaticTypeError] {
      typeInfer(Unary(Cast(TNumber),B(true)))
    }
  }
  
}

class IterateStepSpec extends FlatSpec { 

  "Values" should "evaluate correctly using small-step semantics" in {
    assert(iterateStep(N(1)) == N(1))
    assert(iterateStep(N(-1)) == N(-1))
    assert(iterateStep(B(true)) == B(true))
    assert(iterateStep(B(false)) == B(false))
    assert(iterateStep(S("iubafel")) == S("iubafel"))
    assert(iterateStep(Undefined) == Undefined)
  }

  "Neg" should "evaluate correctly using small-step semantics" in {
    val e1 = Unary(Neg, N(1))
    val e2 = Unary(Neg, N(-2))
    assert(iterateStep(e1) == N(-1))
    assert(iterateStep(e2) == N(2))
  }

  "Not" should "evaluate correctly using small-step semantics" in {
    val e1 = Unary(Not, B(true))
    val e2 = Unary(Not, B(false))
    assert(iterateStep(e1) == B(false))
    assert(iterateStep(e2) == B(true))
  }

  "Seq" should "evaluate correctly using small-step semantics" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = iterateStep(Binary(Seq, e1, e2)) 
    assert(e3 === N(2))
  }

  "Plus" should "evaluate correctly for numbers" in {
    assertResult(N(3)) {
      iterateStep(Binary(Plus, N(1), N(2)))
    }
  }
  it should "evaluate correctly for strings" in {
    assertResult(S("Puppies are cute!")) {
      iterateStep(Binary(Plus, S("Puppies"), S(" are cute!")))
    }
  }

  "Minus" should "evaluate correctly using small-step semantics" in {
    val e1 = iterateStep(Binary(Minus,N(3), N(2)))
    val e2 = iterateStep(Binary(Minus,N(-3), N(2)))
    assert(e1 == N(1))
    assert(e2 == N(-5))
  }

  it should "evaluate correctly when wrapped in Neg" in {
    val e1 = iterateStep(Unary(Neg, Binary(Minus, N(1),N(2))))
    assert(e1 == N(1))
  }

  "Times" should "evaluate correctly using small-step semantics" in {
    val e1 = N(2)
    val e2 = N(4)
    assert(iterateStep(Binary(Times, e1, e2)) == N(8))
  }

  "Div" should "evaluate correctly using small-step semantics" in {
    val e1 = N(8)
    val e2 = N(2)
    assert(iterateStep(Binary(Div, e1, e2)) == N(4))
  }

  "lt" should "evaluate correctly using small-step semantics" in {
    assertResult(B(true)) {
      iterateStep(Binary(Lt, N(0), N(1)))
      iterateStep(Binary(Lt, S("hello"), S("world")))
    }
    assertResult(B(false)) {
      iterateStep(Binary(Lt, N(0), N(0))) 
      iterateStep(Binary(Lt, N(1), N(0))) 
    }
  }

  "le" should "evaluate correctly using small-step semantics" in {
    assertResult(B(true)) {
      iterateStep(Binary(Le, N(0), N(0)))
      iterateStep(Binary(Le, N(0), N(1)))
      iterateStep(Binary(Le, S("hello"), S("world")))
    }
    assertResult(B(false)) {
      iterateStep(Binary(Le, N(1), N(0)))
    }
  }

  "gt" should "evaluate correctly using small-step semantics" in {
    assertResult(B(false)) {
      iterateStep(Binary(Gt, N(0), N(0)))
      iterateStep(Binary(Gt, N(0), N(1)))
      iterateStep(Binary(Gt, S("hello"), S("world")))
    }
    assertResult(B(true)) {
      iterateStep(Binary(Gt, N(1), N(0)))
    }
  }

  "ge" should "evaluate correctly using small-step semantics" in {
    assertResult(B(false)) {
      iterateStep(Binary(Ge, N(0), N(1)))
      iterateStep(Binary(Ge, S("hello"), S("world")))
    }
    assertResult(B(true)) {
      iterateStep(Binary(Ge, N(0), N(0)))
      iterateStep(Binary(Ge, N(1), N(0)))
    }
  }

  "Eq" should "evaluate correctly using small-step semantics" in {
    val x = "x"
    val e1 = N(4)
    val e2 = S("chela")
    assertResult(B(false)) {
      iterateStep(Binary(Eq, e1, e2))
    }
    assertResult(B(true)) {
      iterateStep(Binary(Eq, e1, e1))
      iterateStep(Binary(Eq, e2, e2))
    }
  }

  "Ne" should "evaluate correctly using small-step semantics" in {
    val x = "x"
    val e1 = N(4)
    val e2 = S("chela")
    assertResult(B(true)) {
      iterateStep(Binary(Ne, e1, e2))
    }
    assertResult(B(false)) {
      iterateStep(Binary(Ne, e1, e1))
      iterateStep(Binary(Ne, e2, e2))
    }
  }

  "And" should "evaluate correctly using small-step semantics" in {
    assert(iterateStep(Binary(And, B(false), B(true))) == B(false))
    assert(iterateStep(Binary(And, B(true), B(true))) == B(true))

  }

  "Or" should "evaluate correctly using small-step semantics" in {
    assert(iterateStep(Binary(Or, B(false), B(true))) == B(true))
    assert(iterateStep(Binary(Or, B(true), B(true))) == B(true))
  }

  "if" should "evaluate correctly using small-step semantics" in {
    assert(iterateStep(If(B(true), S("blah"), S("blah2"))) == S("blah"))    
    assert(iterateStep(If(B(false), N(1), N(2))) == N(2))
  }

  "Decl" should "evaluate correctly for constants" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = iterateStep(Decl(MConst, "x", e1, e2)) 
    assert(e3 === N(4))
  }
  it should "evaluate correctly for variables" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = iterateStep(Decl(MVar, "x", e1, e2)) 
    assert(e3 === N(4))
  }

  "Call" should "evaluate non-recursive functions correctly" in { 
    // Simple example
    assertResult(B(true)) {
      iterateStep(
        Call(
          Function(None, Left(List(("param1",TBool),("param2",TBool))), Some(TBool), B(true)),
          List(B(true),B(false))
        )
      )
    }
    // More complex examples
    assertResult(N(3)) {
      iterateStep(
        Call(
          Function(None, Left(List(("param1",TBool),("param2",TBool))), Some(TNumber), Binary(Plus,N(1),N(2))),
          List(B(true), B(false))
        )
      )
    }
    assertResult(N(5)) {
      iterateStep(
        Call(
          Function(None, Left(List(("param1",TBool),("param2",TNumber))), Some(TNumber), Binary(Plus,Var("param2"),N(2))),
          List(B(true), N(3))
        )
      )
    }
  }
  it should "evaluate recursive functions correctly" in {
    assertResult(N(15)) {
      val f = "f"
      val x = "x"
      val fbody = If(Binary(Eq, Var(x), N(0)), 
        Var(x),
        Binary(Plus, 
          Var(x), 
          Call(
            Var(f), 
            List(Binary(Minus, Var(x), N(1)))
            )
          )
        )
      val e1 = Function(Some(f), Left(List(("x", TNumber))), Some(TNumber), fbody)
      iterateStep(Call(e1, List(N(5))))
    }
  }

  "GetField" should "evaluate correctly using small-step semantics" in {
    val obj = Obj(Map(
      ("f1",N(2)),
      ("f2",B(true)),
      ("f3",S("blah")),
      ("f4",Undefined)
    ))
    assertResult(N(2)) {
      iterateStep(GetField(obj, "f1"))
    }
    assertResult(B(true)) {
      iterateStep(GetField(obj, "f2"))
    }
    assertResult(S("blah")) {
      iterateStep(GetField(obj, "f3"))
    }
    assertResult(Undefined) {
      iterateStep(GetField(obj, "f4"))
    }
  }
}