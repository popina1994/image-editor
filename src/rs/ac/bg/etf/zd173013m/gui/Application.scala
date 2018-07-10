package rs.ac.bg.etf.zd173013m.gui

import java.awt.Toolkit._
import java.io.File

import rs.ac.bg.etf.zd173013m.gui.image_label.ImageLabel
import rs.ac.bg.etf.zd173013m.gui.radio_operations.{BoxPanelOperationButtons, ButtonGroupOperations}
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionLayer, ScrollPaneSelectionRectangular, ScrollPaneSelectionSelection}
import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic
import rs.ac.bg.etf.zd173013m.logic.layer.LayerLogic
import rs.ac.bg.etf.zd173013m.logic.operation.OperationsLogic
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionLogic

import scala.swing.{event, _}
import scala.swing.event.{ButtonClicked, ColorChanged, MouseClicked}

object Application extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Image Editor"
    preferredSize = getDefaultToolkit.getScreenSize
    private val imageLabel = new ImageLabel(ImageLogic.DefaultFileName)
    private val scrollPaneSelectionRectangular = new ScrollPaneSelectionRectangular()
    private val scrollPaneSelectionSelection= new ScrollPaneSelectionSelection()
    private val scrollPaneSelectionLayer = new ScrollPaneSelectionLayer()
    private val imageLogic = new ImageLogic(imageLabel, ImageLogic.DefaultFileName, scrollPaneSelectionRectangular)
    private val selectionLogic = new SelectionLogic(imageLogic, scrollPaneSelectionSelection, scrollPaneSelectionRectangular)
    private val buttonGroupOperations = new ButtonGroupOperations
    private val operationsLogic = new OperationsLogic(buttonGroupOperations)
    operationsLogic.listenerOpt = Option(imageLogic)
    private val layerLogic = new LayerLogic(scrollPaneSelectionLayer)

    contents = new FlowPanel {
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
              val result = Dialog.showInput(contents.head, "Input name for new selection", initial="")
              result match {
                case Some(name) => selectionLogic.newSelection(Option(name))
                case None => println("Some random error")
              }
          }
          preferredSize = new Dimension(30, 30)
        }
        contents += new Button {
          text = "Delete"
          reactions += {
            case ButtonClicked(_) => scrollPaneSelectionSelection.deleteSelected()
          }
          preferredSize = new Dimension(20, 30)
        }
      }
      contents += new BoxPanel(Orientation.Vertical) {
        hGap = 3
        vGap = 3
        contents += scrollPaneSelectionLayer.scrollPane
        contents += new Button {
          text = "New"
          reactions += {
            case ButtonClicked(_) =>
              println("Created new layer")
              val result = Dialog.showInput(contents.head, "Input name for new layer", initial="")
              result match {
                case Some(name) => layerLogic.newSelection(Option(name))
                case None => println("Some random error")
              }
          }
          preferredSize = new Dimension(30, 30)
        }
        contents += new Button {
          text = "Delete"
          reactions += {
            case ButtonClicked(_) => scrollPaneSelectionSelection.deleteSelected()
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

        def isEmpty(x: String) = x == null || x.trim.isEmpty
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += new Button {
            text = "Apply"

            reactions += {
              case ButtonClicked(_) =>
                textFieldArg1.text match {
                  // TODO: Checks, dialogs...
                  case arg1 if isEmpty(arg1) => operationsLogic.executeSelectedOperations(None, chosenColor)
                  case arg1 => operationsLogic.executeSelectedOperations(Option(arg1), chosenColor)
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

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Open")
          {
            def apply
            {
              println("Open")
              val chooser = new FileChooser(new File("C:/Users/popina/IdeaProjects/ImageEditor/assets/"))
              val result = chooser.showOpenDialog(null)
              if (result == FileChooser.Result.Approve) {
                println(chooser.selectedFile)
                imageLogic.changeIcon(chooser.selectedFile.getAbsolutePath)
              }
            }
          })
        contents += new MenuItem(new Action("Save") {
          override def apply(): Unit = {
            println("Save")
          }
        })
      }
    }
  }
}
