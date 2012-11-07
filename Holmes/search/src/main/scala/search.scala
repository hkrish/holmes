
import java.io.File
import java.io.FilenameFilter

object search {

	val MAC = 1
	val WIN = 2
	val NIX = 3
	val NUL = -1

	var CONSOLE = 0
  
  def main(args:Array[String]):Unit = {
    val OSNAME = System.getProperty("os.name")
    val OSVERSION = System.getProperty("os.version")

    println("Holmes Searcher version 1.0")
    println(OSNAME + " " + OSVERSION)

    // TODO check os version as well
    // Mac < 10.5? - test
    // Win <= XP? - test
    val macPattern = """(.*\s*OS X\s*)""".r
    val winPattern = """(.*\s+Windows\s*)""".r // ? test
    // val nixPattern = 
    val OS = OSNAME match {
      case macPattern(c) => MAC
      case winPattern(c) => WIN
      case c => NUL
    }

    // Find ConsoleType
    if (OS == MAC || OS == NIX) {
      // TODO Thorough support for coloured terminal types possible
      // priority-least
      val termprog = "(iTerm.*)|(Apple_Terminal)".r
      CONSOLE = System.getenv("TERM_PROGRAM") match {
        case termprog(a, b) => 1
        case _ => {
          val termtype = "(xterm)(-color|-256color){0,1}".r
          System.getenv("TERM") match {
            case termtype(a, b) => 1
            case _ => 0
          }
        }
      }
    }

    pUtil.CONSOLE = CONSOLE

  }
}

/**
 * FileFilter object used to filter signature files.
 */
object signatureFilenameFilter extends FilenameFilter {
  // FIXME Implementation is *imparative
  override def accept(f:File, s:String):Boolean = {
    val n = s.toLowerCase();
    if(n.endsWith(".sig") || n.endsWith(".sigz")) return true;
    return false;
  }
}


/**
 * Object with a collection of helper utilities for printing formatted console output
 */
object pUtil {
  // To clear current line in the console when we need to update status.
  val clearString = Array.fill(80){" "}.mkString
  var CONSOLE = 0

  def printError(str:String) {
		println("[" + clr(0, 31) + "Error" + clr + "]: " + str)
  }
  def printWarn(str:String) {
		println("[" + clr(0, 33) + "Warning" + clr + "]: " + str)
  }
  def printStatus(s1:String, s2:String) {
    print("\r" + clearString + "\r[" + clr(0, 34) + s1 + clr + "]: " + s2)
  }

	/**
   * ANSI coloured output on supported terminals.
   * @param a the style code
   * @param b the color code
   * @return the ANSI escape code.
   */
	def clr(a: Int, b: Int) = {
		if (CONSOLE > 0) "\033[" + a.toString + ";" + b.toString + "m" else ""
	}

  /**
   * Reverts the console colors to normal
   * @return the ANSI escape code to set the console colours to normal.
   */
	def clr = {
		if (CONSOLE > 0) "\033[0m" else ""
	}

  def makeProgressString(ratio:Double,len:Int):String = {
    try{
      // TODO Use fill instead of Stream.continually
      val str = Stream.continually("#").take((ratio * len).toInt).toArray.reduceLeft(_+_)
       "|" + str + Stream.continually(".").take(len - str.length).toArray.reduceLeft(_+_) + "|"
    }catch{
      case e:Exception => "|" + Stream.continually(".").take(len).toArray.reduceLeft(_+_) + "|"
    }
  }

  def printIMNotFound = {
    printError("""Crawler needs ImageMagick to run. Install ImageMagick (http://www.imagemagick.org)""")
      printWarn("Then try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [pathToSignatureDirectory] [pathToCrawl]")
  }
}


