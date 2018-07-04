package rs.ac.bg.etf.zd173013m.gui.image_label

import scala.swing.Point

trait ImageLabelListener {
  def onMouseClick(point: Point)
  def onMouseDrag(point: Point)
  def onMouseRelease(point: Point)
}
