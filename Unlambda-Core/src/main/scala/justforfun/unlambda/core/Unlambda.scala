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

object Unlambda {

  case class ParseExeption( msg:String ) extends Exception
  case class InvalidTreeExeption( msg:String ) extends Exception
  case class ProgramExitException( result:Function ) extends Exception

  abstract class Function

  //Unlambda expression
  abstract class Expression
  case class Application( left:Expression, right:Expression ) extends Expression
  case class SingleFunction( function:Function ) extends Expression
  case class InterpreterMark( evaluating:Expression ) extends Expression

  //Unlambda functions
  case object I extends Function                            //identity function
  case class Dot( char:Char )  extends Function             //"print" function
  case class K1 (function:Function)  extends Function       //Partially applied k
  case object K  extends Function                           //k
  case class S2(f1:Function, f2:Function) extends Function  //Partially applied s (with two args)
  case class S1(f:Function)  extends Function               //Partially applied s (with one arg)
  case object S  extends Function                           //s
  case object V  extends Function                           //v
  case class  D1 (expression:Expression)  extends Function  //applied d
  case object D  extends Function                           //d
  case class  Continuation ( f:Function )  extends Function //applied c
  case object C  extends Function                           //c
  case object E  extends Function                           //e
  case object At  extends Function                          //@
  case class  Ques (char : Char)  extends Function          //?
  case object Pipe  extends Function                        //|
 
  def parse( source:List[Char] ):Expression = {
      def _parse(source:List[Char]) : (Expression, List[Char]) = 
        source match {
          case c::cs => {
            c match {
              case ' ' => _parse(cs)
              case '\r' => _parse(cs)
              case '\n' => _parse(cs)
              case '`' => {
                  val (leftExpression, restOfSorce1) = _parse(cs)
                  val (rightExpression, restOfSorce2) = _parse(restOfSorce1)
                  (Application( leftExpression, rightExpression ), restOfSorce2)
              }
              case 'i' => (SingleFunction(I), cs)
              case 's' => (SingleFunction(S), cs)
              case 'k' => (SingleFunction(K), cs)
              case '.' => ( SingleFunction( Dot(cs.head) ), cs.tail )
              case 'r' => (SingleFunction( Dot('\n') ), cs)
              case 'v' => (SingleFunction( V ), cs)
              case 'c' => (SingleFunction( C ), cs)
              case 'd' => (SingleFunction( D ), cs)
              case 'e' => (SingleFunction( E ), cs)
              case '@' => (SingleFunction( At), cs)
              case '?' => ( SingleFunction(Ques(cs.head) ), cs.tail )
              case '|' => ( SingleFunction(Pipe), cs )
            }
          }
          case _ => {
              throw new IllegalArgumentException("Can't parse Unlambda source")
          }
        }
        val (resultExpression,_) = _parse( source )
        resultExpression
     }
}

class Unlambda( writeOutputFunction:((Char)=>Unit), readInputFunction:(()=>Option[Char]) ) {
  import  Unlambda._;

  var currentChar:Option[Char] = None

  private def apply(f:Function, g:Expression):Function = {
      f match {
        case I => evalInternal(g)
        case Dot(ch) => {
          writeOutputFunction(ch);
          evalInternal(g);
        }
        case K => K1(evalInternal(g))
        case K1(h) => h
        case S => S1( evalInternal(g) )
        case S1(h) => S2(h, evalInternal(g) )
        case S2(x,y) => {
          apply( apply(x, g), SingleFunction(apply(y, g)) )
        }
        case D => D1(g)
        case D1(x) => apply( evalInternal(x), g )
        case V => V
        case C => Continuation( evalInternal(g) )
        case Continuation(f) => evalInternal(g)
        case E => throw ProgramExitException(evalInternal(g))   //Throwing exception here is ugly and dirty but simple :)
        case At => {
          val appllyTo = evalInternal(g)
          currentChar = readInputFunction()
          apply(appllyTo, SingleFunction(I))
        }
        case Ques(charArg) => {
          val appllyTo = evalInternal(g)
          currentChar match {
            case Some(charArg) => apply(appllyTo, SingleFunction(I))
            case _ => apply(appllyTo, SingleFunction(V))
          }
        }
        case Pipe => {
          val appllyTo = evalInternal(g)
          currentChar match {
            case Some(char) => apply(appllyTo, SingleFunction(Dot(char)))
            case None => apply(appllyTo, SingleFunction(V))
          }
        }
        case _ => throw InvalidTreeExeption("Not implemented yet!")
      }
  }

  private def evalInternal(expression:Expression):Function = {
    expression match {
      case SingleFunction(function) => {
        function match {
          case Continuation(f) => f
          case _ => function
        }
      }
      case Application(left, right) => apply( evalInternal(left), right )
      case InterpreterMark(evaluating) => evalInternal(evaluating)  //Ignore interpreter mark
    }
  }

  def eval(expression:Expression):Function = {
    try {
      evalInternal(expression)
    }
    catch {
      case ProgramExitException(result) => result
    }
  }

  private def stepIntoEvalInternal(expression:Expression):Expression = {
    expression match {
      case SingleFunction(function) => expression
      case Application(left, right) => {
        (left, right) match {
          case (SingleFunction(_), InterpreterMark(evaluating)) => evaluating match {
            case Application(_,_) => Application(left, stepIntoEvalInternal(right) )
            case SingleFunction(_) => InterpreterMark( Application(left, evaluating )  )
          }
          case (SingleFunction(_), Application(_,_)) => Application(left, stepIntoEvalInternal(right) )
          case (Application(_,_), SingleFunction(_)) => Application(stepIntoEvalInternal(left), right)
          case (Application(_,_), Application(_,_)) => Application(stepIntoEvalInternal(left), right)
          case (InterpreterMark(evaluating), _) => evaluating match {
            case SingleFunction(_) => Application(evaluating, InterpreterMark(right) )
            case Application(_,_) =>  Application(stepIntoEvalInternal(left), right )
            case InterpreterMark(_) => throw new IllegalArgumentException( "Illegal Unlambda expression tree state" )
          }
          case _ => throw new IllegalArgumentException( "Illegal Unlambda expression tree state" )
        }
      }
      case InterpreterMark(evaluating) => {
        evaluating match {
          case SingleFunction(function) => evaluating
          case Application(left, right) => {
            (left, right) match {
              case (SingleFunction(_), SingleFunction(_) ) => InterpreterMark( SingleFunction( evalInternal(evaluating) ) )
              case (SingleFunction(_), Application(_,_) ) => Application(left, InterpreterMark( right ) )
              case (Application(_,_), _ ) => Application( InterpreterMark(left), right )
              case _ => throw new IllegalArgumentException( "Illegal Unlambda expression tree." )
            }
          }
          case InterpreterMark(_) => throw new IllegalArgumentException( "Illegal Unlambda expression tree. Multiple \"current interpreter position\" marks" )
        }
      }
    }
  }

  def stepIntoEval(expression:Expression):Expression = {
    try {
      stepIntoEvalInternal(expression)
    }
    catch {
      case ProgramExitException(result) => SingleFunction(result)
    }
  }

}
