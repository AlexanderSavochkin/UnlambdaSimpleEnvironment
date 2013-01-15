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

package justforfun.unlambda.console

import justforfun.unlambda.core.Unlambda

object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val sourceFileName = args(0);
    val source = scala.io.Source.fromFile(sourceFileName, "utf-8").getLines.mkString
    //val exp = Unlambda.parse( "```s``s``sii`ki`k.*``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`kr``s`k`sikk`k``s`ksk".toList ) //Fibonacci numbers example
    val exp = Unlambda.parse( source.toList )
    val unlambda = new Unlambda( print, () => Console.in.read match {
      case -1 => None
      case ch => Some(ch.toChar)
    } );
    unlambda.eval(exp)
  }

}
