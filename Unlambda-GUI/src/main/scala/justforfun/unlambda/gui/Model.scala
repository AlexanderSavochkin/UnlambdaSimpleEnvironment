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

 Unlambda interpreter "GUI" in Scala programming language
*/

package justforfun.unlambda.gui

import justforfun.unlambda.core.Unlambda

class Model {

  var currentUnlambdaSource:String = ""
  var currentState:Option[Unlambda.Expression] = None
  var output:StringBuffer = new StringBuffer
  var input:String = ""
  val unlambdaInterpreter = new Unlambda( (c:Char)=>output.append(c),
    ()=>{
        if (input.length != 0) {
          val res = input.charAt(0)
          input = input.substring(1);
          Some(res)
        }
        else None
    } )

  def reset:Unit = try {
    currentState =  Some(Unlambda.InterpreterMark( Unlambda.parse(currentUnlambdaSource.toList) ) )
  }
  catch {
    case e:Exception => currentState =  None
  }

  def stepInto:Unit = {
    currentState match {
      case Some(expressionTree) => {
        val nextState = unlambdaInterpreter.stepIntoEval( expressionTree )
        currentState = Some(nextState)
        }
      case None => {}
    }
  }

//  def stepOver:Unit = {
//    currentState match {
//      case Some(expressionTree) => {
//        val nextState = unlambdaInterpreter.stepOverEval( expressionTree )
//        currentState = Some(nextState)
//        }
//      case None => {}
//    }
//  }

  def setUnlambdaSource(source:String):Unit = {
    currentUnlambdaSource = source
    reset
  }

  def getCurrentState():Option[Unlambda.Expression] = currentState


  def fetchAndClearOutput():String = {
    val result = output.toString
    output = new StringBuffer
    return result
  }

}
