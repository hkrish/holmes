
import java.io.InputStreamReader
import java.io.InputStream
import scala.io.Source
import java.io.File
import java.io.FilenameFilter
import org.im4java.process
import org.im4java.process._
import org.im4java.process.ProcessStarter
import org.im4java.core
import org.im4java.core._
import 

object search {
	val MAC = 1
	val WIN = 2
	val NIX = 3
	val NUL = -1

  def main(args:Array[String]):Unit = {
    /*
     * retrieve options as a map
     */
    val options = parseArgs(args)

    /**
     * Setup IM4Java and other variables
     */
    val pathIM = doPreCheck
    pUtil.CONSOLE = 1
    ProcessStarter.setGlobalSearchPath(pathIM);

		if (args.length < 2) {
			// Not enough Program Arguments
			pUtil.printError("Try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [" + (if(args.length < 1)pUtil.clr(0, 31) else "") +
      "pathToSignatureDirectory"+ pUtil.clr + "] ["+ pUtil.clr(0, 31)+"pathToCrawl"+pUtil.clr+"]")
			println("Bye!")
		} else {
      val  targetDir = if(args(1).endsWith(File.separator)) args(1) else args(1) + File.separator;
      
      if(!(new File(args(0)).exists)) {
        pUtil.printError("""Target directory doesnot exist or is invalid.""")
        println("Bye!")
        sys.exit(0) // TODO Safe?
      }

      val sigd = new File(targetDir)
      val sigList = if(sigd.exists && sigd.isDirectory){
        sigd.list(signatureFilenameFilter).map(_.split("\\.")(0).toInt)
      }else {
        pUtil.printError("""Signature directory doesnot exist or is invalid.""")
        println("Bye!")
        sys.exit(0) // TODO Safe?
        // sys.exit should be safe till this point because  
        //
        // we havn't started any threads or opened any streams yet.
      }

			// Ok now lets do our stuff
      try{
        /**
         * Prepare a command for making the thumbnails
         */
        val cmd = new ConvertCmd()
        val op = new IMOperation()
        op.addImage();
        op.resize(256, 256)
        op.addImage("JPEG:-")
      } catch {
        case e:Exception => e.printStackTrace
      }
    }
  }

  def parseArgs(args:Array[String]):Array[String] = {
    args
  }

  def doPreCheck() = {
    println("Holmes Searcher version 1.0")

		// Try to find ImageMagick
    val magickHome = try{
      System.getenv("MAGICK_HOME")
    }catch{ 
      case e:Exception => e.printStackTrace()
      ""
    } 

    val pathIM = if(magickHome != "")
      (if(magickHome.endsWith(File.separator)) magickHome else magickHome + File.separator) + "bin" + File.separator
    else ""

    return pathIM
  }

  def convertStreamToString(is: InputStream): Option[String] = try {
		val src = Source.fromInputStream(is).getLines
		if (src.isEmpty) None else Some(src.reduceLeft(_ + "\n" + _))
	} catch {
		case e: Exception => {
			e.printStackTrace()
			None
		}
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
    printError("""Crawler needs ImageMagick to run. Install ImageMagick (http://www.imagemagick.org)\nand set the MAGICK_HOME environment
      variable""")
      printWarn("Then try: search [pathToSignatureDirectory] [pathToCrawl]")
  }
}

