package GrowCode

object Marshall {

  def writeToFile( fname: String, obj : Serializable ) {
    val fos = new java.io.FileOutputStream( fname )
    val oos = new java.io.ObjectOutputStream( fos )
    oos.writeObject( obj )
    oos.close()
  }

  def read[T <: Serializable]( fname: String ) = {
    val fis = new java.io.FileInputStream( fname )
    val ois = new java.io.ObjectInputStream(fis)
    val o = ois.readObject().asInstanceOf[T]
    ois.close()
    o
  }

}
