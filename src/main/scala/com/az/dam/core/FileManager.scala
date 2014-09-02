package com.az.dam.core
import java.io.InputStream
import java.io.{File, BufferedReader, InputStreamReader}

/**
 * Created by ankit on 8/14/2014.
 */
class FileManager {

  def readFile(filename:String, callback: String => Unit ): Unit = {
      val bufferedReader = new BufferedReader(new InputStreamReader(new FileManager().readStream(new File(filename))))
      val line = bufferedReader.readLine;
      if (line != null) {
        callback(line);
      }
  }

  def readStream(file: File): InputStream = {
    readStream(file, maxRetries, sleep(waitToOpen), sleep(waitBetweenReads))

    def readStream(file: File, openTries: Int, openSleep: () => Unit, rereadSleep: () => Unit): InputStream = {
      import java.io.SequenceInputStream

      val e = new java.util.Enumeration[InputStream]() {
        def nextElement = new TailInputStream(file, rereadSleep)
        def hasMoreElements = testExists(file, openTries, openSleep)
      }

      new SequenceInputStream(e)
    }
  }

  def testExists(file: File, tries: Int, sleep: () => Unit): Boolean = {
    def tryExists(n: Int): Boolean =
      if (file.exists) true
      else if (n > tries) false
      else {
        sleep()
        tryExists(n + 1)
      }

    tryExists(1)
  }
}

class TailInputStream(val file: File, val waitNewInput: () => Unit) extends InputStream {

  import java.io.FileInputStream

  private val tailInputStream = new FileInputStream(file)

  def read: Int = handle(tailInputStream.read)

  override def read(b: Array[Byte]): Int = read(b, 0, b.length)

  override def read(byte: Array[Byte], offset: Int, length: Int): Int = handle(tailInputStream.read(byte, offset, length))

  override def close = tailInputStream.close

  protected def rotated_? = try {
    tailInputStream.getChannel.position > file.length
  }
  finally {
    false
  }

  protected def closed_? = !tailInputStream.getChannel.isOpen

  protected def handle(read: => Int): Int = read match {
    case -1 if rotated_? || closed_? => -1
    case -1 =>
      waitNewInput()
      handle(read)
    case i => i
  }

  require(file != null)
  assume(file.exists)

}

object FileManager {
  def main(args: Array[String]): Unit = {
    val fileManager = new FileManager();
    fileManager.readFile("c:/log1.log", callback);
  }

  //For each new line read from file callback is called
  def callback(line:String): Unit = {
    println(line);
  }
}




