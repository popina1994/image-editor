package rs.ac.bg.etf.zd173013m.gui.application

import java.awt.Toolkit._
import java.io.File

import rs.ac.bg.etf.zd173013m.gui.image_label.ImageLabel
import rs.ac.bg.etf.zd173013m.gui.radio_operations.ButtonGroupOperations
import rs.ac.bg.etf.zd173013m.gui.scroll_pane.{ScrollPaneSelectionLayer, ScrollPaneSelectionRectangular, ScrollPaneSelectionSelection}
import rs.ac.bg.etf.zd173013m.logic.image.ImageLogic
import rs.ac.bg.etf.zd173013m.logic.layer.LayerLogic
import rs.ac.bg.etf.zd173013m.logic.operation.OperationsLogic
import rs.ac.bg.etf.zd173013m.logic.selection.SelectionLogic

import scala.swing._

object Application extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Image Editor"
    preferredSize = getDefaultToolkit.getScreenSize
    val buttonGroupOperations = new ButtonGroupOperations

    var flowPanelApplication: FlowPanelApplication = null

    def initApplication(fileName: String): FlowPanelApplication= {
      val imageLabel = new ImageLabel(fileName)
      // UPDATE
      val scrollPaneSelectionRectangular = new ScrollPaneSelectionRectangular()
      // UPDATE
      val scrollPaneSelectionSelection = new ScrollPaneSelectionSelection()
      val scrollPaneSelectionLayer = new ScrollPaneSelectionLayer()
      // UPDATE
      val imageLogic = new ImageLogic(imageLabel, fileName, scrollPaneSelectionRectangular, scrollPaneSelectionLayer)
      // UPDATE
      val selectionLogic = new SelectionLogic(imageLogic, scrollPaneSelectionSelection, scrollPaneSelectionRectangular)
      // STAYS, maybe?
      val operationsLogic = new OperationsLogic(buttonGroupOperations, scrollPaneSelectionLayer)
      operationsLogic.listenerOpt = Option(imageLogic)
      // UPDATE
      val layerLogic = new LayerLogic(scrollPaneSelectionLayer)
      layerLogic.layerChangeListener = Option(imageLogic)

      flowPanelApplication = new FlowPanelApplication(scrollPaneSelectionRectangular, scrollPaneSelectionSelection,
        scrollPaneSelectionLayer, imageLogic, selectionLogic, layerLogic, imageLabel, buttonGroupOperations, operationsLogic)
      return flowPanelApplication
    }


    def updateContentsMainFrame(newComponent: Component) {
      contents = newComponent
    }
    updateContentsMainFrame(initApplication(ImageLogic.DefaultFileName))
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("New")
        {
          def apply
          {
            println("New")
            updateContentsMainFrame(initApplication(ImageLogic.DefaultFileName))
          }
        })
        contents += new MenuItem(new Action("Open")
          {
            def apply
            {
              println("Open")
              val chooser = new FileChooser(new File("C:/Users/popina/IdeaProjects/ImageEditor/assets/"))
              val result = chooser.showOpenDialog(null)
              if (result == FileChooser.Result.Approve) {
                println(chooser.selectedFile)
                updateContentsMainFrame(initApplication(chooser.selectedFile.getAbsolutePath))
              }
            }
          })
        contents += new MenuItem(new Action("Save") {
          override def apply(): Unit = {
            val chooser = new FileChooser(new File("C:/Users/popina/IdeaProjects/ImageEditor/assets/"))
            val result = chooser.showSaveDialog(null)
            if (result == FileChooser.Result.Approve) {
              println(chooser.selectedFile)
              try {
                flowPanelApplication.imageLogic.saveImage(chooser.selectedFile.getAbsolutePath)
              }
              catch {
                case exc: Exception=>
                  println("Error during saving")
              }
            }
          }
        })
      }
    }
  }
}
