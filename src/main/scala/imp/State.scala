package imp

class State(map: Map[String, Val]) {

  def this() = this(Map())

  def read(n: String): Option[Val] = map.get(n)

  def updt(n: String, v: Val): State = {
    new State(map.updated(n, v))
  }

  override def toString = s"State($map)"

}
