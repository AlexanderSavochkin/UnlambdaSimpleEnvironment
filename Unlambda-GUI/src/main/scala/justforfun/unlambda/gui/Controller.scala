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

import swing.{Action}
import swing.event.Key
import javax.swing.KeyStroke

class Controller(model:Model, postReset: ()=>Unit, postUpdate: ()=>Unit, getCurrentSource:()=>String, getCurrentInput:()=>String) {

  def getResetAction = new Action("Reset") {
    accelerator = Some( KeyStroke.getKeyStroke("F2") )
    def apply() {
      model.currentUnlambdaSource = getCurrentSource()
      model.reset
      postReset()
    }
  }

  def getStepIntoAction = new Action("Step into") {
    accelerator = Some( KeyStroke.getKeyStroke("F7") )
    def apply() {
      model.input = getCurrentInput()
      model.stepInto
      postUpdate()
    }
  }

/*  def getStepOverAction = new Action("Step over") {
    accelerator = Some( KeyStroke.getKeyStroke("F8") )
    def apply() {
      model.stepOver
      postUpdate()
    }
  }*/

}
