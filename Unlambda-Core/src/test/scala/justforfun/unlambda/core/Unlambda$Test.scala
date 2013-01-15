/*
 Copyright (C) 2012, Alexander Savochkin

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.

 Unlambda interpreter "core" in Scala programming language
*/

package justforfun.unlambda.core

import org.scalatest.FunSuite
import justforfun.unlambda.core.Unlambda._

class Unlambda$Test extends FunSuite {

  val unlambda = new Unlambda( print, ()=>Some(Console.in.read.toChar) )

  //Parse tests
  test("Unlambda source parse test 1") {
    val exp = Unlambda.parse("`sk".toList)
    println(exp)
    assert( exp == Application(SingleFunction(S),SingleFunction(K)) )
  }

  test("Unlambda source parse test 2") {
    val exp = Unlambda.parse("``ski".toList)
    println(exp)
    assert( exp == Application(Application(SingleFunction(S),SingleFunction(K)),SingleFunction(I)) )
  }



  //Evaluation tests
  test("Unlambda evaluation test 1") {
    val exp = Unlambda.parse("```skki".toList)  //``skk must be identity function (```skkx = x fro any x)
    print(exp.toString + " = ")
    val result = SingleFunction(unlambda.eval(exp))
    println( result )
    assert( result == SingleFunction(I) )
  }

  test("Unlambda evaluation test 2") {
    val exp = Unlambda.parse("````skkki".toList)
    print(exp.toString + " = ")
    val result = SingleFunction(unlambda.eval(exp))
    println( result )
    assert( result == SingleFunction(K1(I)) )
  } 

  test("Unlambda evaluation test 3") {
    val exp = Unlambda.parse("`s`ei".toList)  //`s`ei must be evaluated to i
    print(exp.toString + " = ")
    val result = SingleFunction(unlambda.eval(exp))
    println( result )
    assert( result == SingleFunction(I) )
  }


  //Step by step tests
  test("Unlambda step by step test 1") {
    val exp = Unlambda.parse("``kik".toList)  //result must be i
    val markedExp = InterpreterMark(exp)
    println( markedExp )
    val after1Step = unlambda.stepIntoEval(markedExp)
    println( after1Step )
    assert( after1Step == Application(InterpreterMark(Application(SingleFunction(K),SingleFunction(I))),SingleFunction(K)) )
  }

  test("Unlambda step by step test 2") {
    val markedExp = InterpreterMark( Application( SingleFunction(K1(I)), SingleFunction(K) ) )
    val after1Step = unlambda.stepIntoEval(markedExp)
    println( after1Step )
    assert( after1Step == InterpreterMark( SingleFunction(I) ) )
  }

  test("Unlambda step by step test 3") {
    val markedExp = InterpreterMark( SingleFunction(I) )
    val after1Step = unlambda.stepIntoEval(markedExp)
    println( after1Step )
    assert( after1Step == SingleFunction(I) )
  }

  test("Unlambda step by step test 4") {
    val markedExp = InterpreterMark( SingleFunction(I) )
    val after1Step = unlambda.stepIntoEval(markedExp)
    println( after1Step )
    assert( after1Step == SingleFunction(I) )
  }

  test("Unlambda step by step test 5") {
    val markedExp = Application( InterpreterMark( SingleFunction(I) ), SingleFunction(I))
    val after1Step = unlambda.stepIntoEval(markedExp)
    println( after1Step )
    assert( after1Step == Application( SingleFunction(I), InterpreterMark( SingleFunction(I))) )
  }

  test("Unlambda step by step test 6") {
    val markedExp = Application( SingleFunction(I), InterpreterMark( SingleFunction(I) ) )
    val after1Step = unlambda.stepIntoEval(markedExp)
    println( after1Step )
//    assert( after1Step == InterpreterMark( Application( SingleFunction(I), SingleFunction(I))))
  }

}
