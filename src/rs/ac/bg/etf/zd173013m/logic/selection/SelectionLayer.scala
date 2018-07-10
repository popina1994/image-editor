package rs.ac.bg.etf.zd173013m.logic.selection

class SelectionLayer (_name: String) extends Selection(_name) {
  def this()= this("Layer" + SelectionLayer.generateId().toString)
}

object SelectionLayer {
  var id: Int = 0
  def generateId(): Int=
  {
    id +=1
    return id
  }
}
