package rs.ac.bg.etf.zd173013m.gui.application

import rs.ac.bg.etf.zd173013m.gui.image_label.ImageLabel
import rs.ac.bg.etf.zd173013m.gui.radio_operations.{BoxPanelOperationButtons, ButtonGroupOperations}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionLayer, ScrollPaneSelectionRectangular, ScrollPaneSelectionSelection}
import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic
import rs.ac.bg.etf.zd173013m.logic.layer.LayerLogic
import rs.ac.bg.etf.zd173013m.logic.operation.OperationsLogic
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionLogic

import scala.swing.event.{ButtonClicked, ColorChanged, ValueChanged}
import scala.swing.{BoxPanel, Button, CheckBox, Color, ColorChooser, Dialog, Dimension, FlowPanel, Label, Orientation, ScrollPane, Slider, TextArea}

class FlowPanelApplication(scrollPaneSelectionRectangular: ScrollPaneSelectionRectangular,
                           scrollPaneSelectionSelection: ScrollPaneSelectionSelection,
                           scrollPaneSelectionLayer: ScrollPaneSelectionLayer,
                           imageLogic: ImageLogic, selectionLogic: SelectionLogic,
                           layerLogic: LayerLogic,
                           imageLabel: ImageLabel,
                           buttonGroupOperations: ButtonGroupOperations,
                           operationsLogic: OperationsLogic) extends FlowPanel {
  def isEmpty(x: String) = x == null || x.trim.isEmpty
  contents += new BoxPanel(Orientation.Vertical) {
    hGap = 3
    vGap = 3
    contents += scrollPaneSelectionLayer.scrollPane
    contents += new Button {
      text = "New"
      reactions += {
        case ButtonClicked(_) =>
          println("Created new layer")
          var result = Dialog.showInput(contents.head, "Input name for new layer", initial="")
          result = if (isEmpty(result.get)) (None) else result
          layerLogic.newSelection(result)
      }
      preferredSize = new Dimension(30, 30)
    }
    contents += new Button {
      text = "Delete"
      reactions += {
        case ButtonClicked(_) => scrollPaneSelectionLayer.deleteSelected()
      }
      preferredSize = new Dimension(20, 30)
    }
    contents += new Slider  {
      min = 0
      max = LayerLogic.MaxSlider
      value = LayerLogic.MaxSlider
      minorTickSpacing = 5
      preferredSize = new Dimension(100, 30)
      paintTicks = true

      reactions += {
        case ValueChanged(a) =>
          layerLogic.updateTransparency(value)
      }
    }
  }

  contents += imageLabel
  contents += new BoxPanel(Orientation.Vertical) {
    hGap = 3
    vGap = 3
    contents += scrollPaneSelectionRectangular.scrollPane
    contents += new Button {
      text = "Delete"
      reactions += {
        case ButtonClicked(_) => imageLogic.deleteSelected()
      }
      preferredSize = new Dimension(20, 30)
    }
  }

  contents += new BoxPanel(Orientation.Vertical) {
    hGap = 3
    vGap = 3
    contents += scrollPaneSelectionSelection.scrollPane
    contents += new Button {
      text = "New"
      reactions += {
        case ButtonClicked(_) =>
          println("Created new selection")
          var result = Dialog.showInput(contents.head, "Input name for new selection", initial="")
          result = if (isEmpty(result.get)) (None) else result
          selectionLogic.addNewSelection(result)
      }
      preferredSize = new Dimension(30, 30)
    }
    contents += new Button {
      text = "Delete"
      reactions += {
        case ButtonClicked(_) => selectionLogic.deleteSelected()
      }
      preferredSize = new Dimension(20, 30)
    }
  }

  contents += new BoxPanel(Orientation.Vertical) {
    val boxPanelOperationButtons = new  BoxPanelOperationButtons(Orientation.Vertical, buttonGroupOperations.buttons)
    buttonGroupOperations.listnerRadioButtonAdded = Option(boxPanelOperationButtons)
    operationsLogic.listenersSeqOp += buttonGroupOperations
    contents += new ScrollPane() {
      contents = boxPanelOperationButtons
      preferredSize = new Dimension(100, 330)
    }
    val textFieldArg1 = new TextArea() {
      preferredSize = new Dimension(30, 50)
    }
    var chosenColor: Option[Color] = None
    val colorChooser = new ColorChooser {
      reactions += {
        case ColorChanged(_, c) =>
          chosenColor = Option(c)
      }
    }


    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Button {
        text = "Apply"

        reactions += {
          case ButtonClicked(_) =>
            try {
              textFieldArg1.text match {
                // TODO: Checks, dialogs...
                case arg1 if isEmpty(arg1) => operationsLogic.executeSelectedOperations(None, chosenColor)
                case arg1 => operationsLogic.executeSelectedOperations(Option(arg1), chosenColor)

              }
            }
            catch  {
              case exception: Exception=>
                println("Wrong argument")
            }
        }
        println("Applied operation")
      }

      val buttonNext = new Button {
        text = "Next"
        reactions += {
          case ButtonClicked(_) =>
            textFieldArg1.text match {
              case arg1 if isEmpty(arg1) => operationsLogic.saveSelectedOperations(None, chosenColor)
              case arg1 => operationsLogic.saveSelectedOperations(Option(arg1), chosenColor)
            }
        }
        visible = false
      }
      val checkBoxComposite = new CheckBox("Composite function") { visible = true}

      contents += new Button {
        var textId = 0
        var saveOperation = false
        val textArray = List("Record", "Stop")
        text = "Record"
        reactions += {
          case ButtonClicked(_) =>
            textId = (textId + 1) % textArray.length
            buttonNext.visible = !buttonNext.visible
            checkBoxComposite.visible = !checkBoxComposite.visible
            if (saveOperation)
            {
              textFieldArg1.text match {
                case arg1 if isEmpty(arg1) => operationsLogic.createSequenceOperations(None, checkBoxComposite.selected)
                case arg1 => operationsLogic.createSequenceOperations(Option(arg1), checkBoxComposite.selected)
              }
            }
            saveOperation = !saveOperation
            text = textArray(textId)
        }
      }
      contents += buttonNext
      contents += checkBoxComposite
    }
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new Label("First arg:")
      contents += textFieldArg1
    }
    contents +=  colorChooser
  }
}
