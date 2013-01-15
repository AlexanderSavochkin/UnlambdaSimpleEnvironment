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
import justforfun.unlambda.core.Unlambda
import swing.tree.{TreeModel, Tree}
import scala.Some

trait UnlambdaTreeWrapper {
  def getChildren: List[UnlambdaTreeWrapper]
}

class ValidTreeWrapper(node: Unlambda.Expression) extends UnlambdaTreeWrapper {
  val children = node match {
    case Unlambda.SingleFunction(_) => Nil
    case Unlambda.Application(left, right) => List(new ValidTreeWrapper(left), new ValidTreeWrapper(right)) //TODO: if left or right is InterpreterMark, change it with munder-mark subtree
    case Unlambda.InterpreterMark(exp) => List(new ValidTreeWrapper(exp)) //TODO: What if interpreter mark is root?
  }

  override def toString = node match {
    case Unlambda.Application(_, _) => "`"
    case Unlambda.SingleFunction(f) => f.toString()
    case Unlambda.InterpreterMark(_) => "*"
  }

  def getChildren = children
}

class InvalidTreeWrapper extends UnlambdaTreeWrapper {
  override def toString = "Invalid unlambda expression..."

  def getChildren = Nil
}


class ViewFrame(_unlambdaModel: Model) extends MainFrame {

  val unlambdaModel: Model = _unlambdaModel

  title = "Unlambda GUI interpreter"

  def treeModelFromCurrentState(): TreeModel[UnlambdaTreeWrapper] =
    TreeModel(
      unlambdaModel.getCurrentState() match {
        case None => new InvalidTreeWrapper().asInstanceOf[UnlambdaTreeWrapper]
        case Some(e) => new ValidTreeWrapper(e).asInstanceOf[UnlambdaTreeWrapper]
      })(_.getChildren)

  //Create and adjust unlambda source text area
  val programTextArea = new TextArea()

  //Create and adjust unlambda tree view
  val programTreeView = new Tree[UnlambdaTreeWrapper] {
    model = treeModelFromCurrentState()
    selection.mode = Tree.SelectionMode.Single
    renderer = Tree.Renderer((x: UnlambdaTreeWrapper) => {
      x.toString
    })
  }
  programTreeView.expandAll();

  //Create and adjust unlambda output
  val outputTextArea = new TextArea()
  outputTextArea.editable = false;

  //Create and adjust unlambda input
  val inputTextArea = new TextArea()


  //Create and adjust splitters
  //Input-Output splitter
  val splitInputOutput = new SplitPane(Orientation.Horizontal,
    new BoxPanel(Orientation.Vertical) {
      contents += new Label("Input")
      contents += new ScrollPane() {
        contents = inputTextArea
      }
    },
    new BoxPanel(Orientation.Vertical) {
      contents += new Label("Output")
      contents += new ScrollPane() {
        contents = outputTextArea
      }
    })
  splitInputOutput.dividerLocation = 120

  //Source from input-output splitter
  val splitProgramIO = new SplitPane(Orientation.Horizontal,
    new BoxPanel(Orientation.Vertical) {
      contents += new Label("Source code")
      contents += new ScrollPane() {
        contents = programTextArea
      }},
    splitInputOutput)
  splitProgramIO.dividerLocation = 120
  contents = splitProgramIO

  //Tree/Other splitter
  val splitTreeAndOthers = new SplitPane(Orientation.Vertical,
    splitProgramIO,
    new BoxPanel(Orientation.Vertical) {
      contents += new Label("Evaluation tree")
      contents += new ScrollPane() {
        contents = programTreeView
      }
    }
  )
  splitTreeAndOthers.dividerLocation = 300

  //
  val controller = new Controller(unlambdaModel, () => this.reset, () => this.update, () => programTextArea.text, () => inputTextArea.text);

  val resetAction = controller.getResetAction
  val stepIntoAction = controller.getStepIntoAction
  //val stepOverAction = controller.getStepOverAction
  val exitAction = new Action("Exit") {
    def apply() {
      System.exit(0)
    }
  }

  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(exitAction)
    }
    contents += new Menu("Run&Debug") {
      contents += new MenuItem(resetAction)
      contents += new MenuItem(stepIntoAction)
      //contents += new MenuItem( stepOverAction )
    }
    contents += new Menu("Help")
  }

  contents = splitTreeAndOthers

  def reset = {
    outputTextArea.text = ""
    update
  }

  //Get data from model and put it here
  def update = {
    //Replace tree content
    programTreeView.model = treeModelFromCurrentState()
    programTreeView.expandAll()

    //update output content
    outputTextArea.append(unlambdaModel.fetchAndClearOutput)
    inputTextArea.text = unlambdaModel.input
  }

}
