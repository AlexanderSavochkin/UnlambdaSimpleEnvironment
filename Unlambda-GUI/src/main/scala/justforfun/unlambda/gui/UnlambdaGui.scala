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

import swing._

object UnlambdaGui extends SimpleSwingApplication {

  def top = new ViewFrame( new Model ) {
    size = new Dimension(600,400)
  }

}
