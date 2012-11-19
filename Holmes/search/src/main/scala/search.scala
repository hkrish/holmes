
import java.io.ByteArrayOutputStream
import java.io.InputStreamReader
import java.io.InputStream
import scala.io.Source
import scala.math._
import java.io.File
import java.io.FileFilter
import org.im4java.process
import org.im4java.process._
import org.im4java.process.ProcessStarter
import org.im4java.core
import org.im4java.core._
import org.apache.commons.cli._

object search {
	val MAC = 1
	val WIN = 2
	val NIX = 3
	val NUL = -1

  /*
   * Make options
   */
  val parser:CommandLineParser = new PosixParser
  val options:Options = new Options
  options.addOption( "l", "level", true, "Search level 1=prefilter, 2=preliminary, 3=thorough[default]." )
  options.addOption( "s", "signature", true, "the directory that contains signatures [.sig" + File.separator + "]" )

  var level:Int = 3
  var cutoffBeta:Int = 50
  var cutoff1:Double = 0.5
  var cutoff2:Double = 0.25
  var sigDir = ".sig"
  var searchImg:WaveletSignature = _

  def main(args:Array[String]):Unit = {
      search(args) match {
        case Some(a) => a.map(a => println(a._2 + ":" + a._1.fileName))
        case _ => pUtil.printStatus("search", "No results")
      }
  }

  def search(args:Array[String]):scala.Option[List[(WaveletSignature, Double)]] = {
    /**
     * Setup IM4Java and other variables
     * TODO: move commandline parsing to the main method
     */
    println("Holmes Searcher version 1.0")
    pUtil.CONSOLE = 1
    ProcessStarter.setGlobalSearchPath(System.getenv("PATH"))

    try {
      val line:CommandLine = parser.parse( options, args )
      sigDir = new File(if(line.hasOption( "signature" )) line.getOptionValue("signature") else sigDir).getAbsolutePath + File.separator
      level = if(line.hasOption( "level" )) line.getOptionValue("level").toInt else level

      // Prepare the search image
      val leftover = line.getArgs
      if(leftover.length > 0){
        searchImg = getWaveletSignature(new File(leftover(0)).getAbsolutePath) match {
          case Some(a) => a
          case _ => {
            pUtil.printError("no valid image to Search for")
            sys.exit(0)
          }
        }
      }else{
        pUtil.printError("no valid image to Search for")
        sys.exit(0)
      }
    }catch{
      case e:ParseException => pUtil.printError("Error while parsing options :" + e.getMessage())
    }
 
    val sigd = new File(sigDir)
    if(!sigd.exists || !sigd.isDirectory){
      pUtil.printError("""Signature directory doesnot exist. Use crawler first.""")
      println("Bye!")
      sys.exit(0)
      // sys.exit should be safe till this point because 
      // we havn't started any threads or opened any streams yet.
    }

    val sigList = sigd.listFiles(signatureFileFilter).map(a => WaveletSignatureFS.loadFromGZ(a.getAbsolutePath))
 
    // Ok now lets do our stuff
    try{
      searchImg.initSignature(cutoffBeta)

      val firstList = sigList.filter(sig => FeatureVectorutil.checkSigmaWith(searchImg, sig))
      val secondList = if(level > 1){
        val al = firstList.map(sig => (sig, FeatureVectorutil.getFirstMatrixDistanceWith(searchImg, sig))).sortBy(_._2).toList
        al.take(round(al.length * cutoff1).toInt)
      }else
        Nil

      val resultList:List[(WaveletSignature, Double)] = if(level > 2){
        val al = secondList.map(sig => (sig._1, FeatureVectorutil.getDistanceWith(searchImg, sig._1))).sortBy(_._2).toList
        al.take(round(al.length * cutoff2).toInt)
      }else if(!secondList.isEmpty)
        secondList
      else
        firstList.toList.map((_, 0.0))

      Some(resultList)
    } catch {
      case e:Exception => e.printStackTrace
      None
    }
  }

  def getWaveletSignature(fileName:String):scala.Option[WaveletSignature] = {
    val cmd = new ConvertCmd()
    val op = new IMOperation()
    op.addImage(fileName);
    op.resize(256, 256, "!")
    op.colorspace("YUV")
    op.separate()
    op.appendVertically()
    op.size(256, 768)
    op.depth(8)
    op.addImage("GRAY:-")

    try {
      // Read the output of the IM command as a stream into memory
      val bos = new ByteArrayOutputStream(196608);
      val pipeOut:Pipe = new Pipe(null,bos);
      cmd.setOutputConsumer(pipeOut)
      cmd.run(op)

      val buff = bos.toByteArray();
      bos.close()
      
      val iBuff = buff.map(_.toInt).toArray
      if(iBuff.length >= 196608){
        Some(new WaveletSignature(iBuff))
      } else
        None
    } catch {
      case e: Exception => { 
        e.printStackTrace()
        None
      }
    }
  }
}


/**
 * FileFilter object used to filter signature files.
 */
object signatureFileFilter extends FileFilter {
  // FIXME Implementation is *imparative
  override def accept(f:File):Boolean = {
    val n = f.getName.toLowerCase();
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

