
import java.io.BufferedReader
import scala.actors.Actor
import scala.actors.Actor._
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.InputStreamReader
import java.io.InputStream
import scala.io.Source
import java.io.File
import java.io.FileFilter
import java.io.FilenameFilter
import org.im4java.process
import org.im4java.process._
import org.im4java.process.ProcessStarter
import org.im4java.core
import org.im4java.core._

/**
 * crawl 
 *    A Crawler for walking the filesystem and create/update wavelet signatures for 
 *    certain 2d graphic files.
 */
object crawl {

	val MAC = 1
	val WIN = 2
	val NIX = 3
	val NUL = -1

	var CONSOLE = 0
  
	def main(args: Array[String]): Unit = {
		val OSNAME = System.getProperty("os.name")
		val OSVERSION = System.getProperty("os.version")

		println("Holmes Crawler version 1.0")
		println(OSNAME + " " + OSVERSION)

		// TODO check os version as well
		// Mac < 10.5? - test
		// Win <= XP? - test
		val macPattern = """(.*\s*OS X\s*)""".r
		val winPattern = """(.*\s*Windows\s*.*)""".r // ? test
		// val nixPattern = 
		val OS = OSNAME match {
			case macPattern(c) => MAC
			case winPattern(c) => WIN
			case c => NUL
		}

		if (OS == NUL) {
			println("Unsupported OS")
			println("Bye!")
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
		
		// Try to find ImageMagick
		val imCheck:Process = if(OS == MAC || OS == NIX) new ProcessBuilder("which", "convert").start() else new ProcessBuilder("cmd.exe",
      "where", "convert").start()
		val imCheckOut = convertStreamToString(imCheck.getInputStream())
		imCheck.waitFor()

		val pathIM = if (System.getProperty("PATH_IM") == null) {
			val matchstr = File.separator + "convert"
			val whichstr = ("""(.*)(""" + matchstr + ")").r;
			imCheckOut match {
				case Some(a) => a match {
					case whichstr(a, matchstr) => a + File.separator
					case _ => {
						pUtil.printIMNotFound
						sys.exit(0)
						""
					}
				}
				case None => {
					pUtil.printIMNotFound
					sys.exit(0)
					""
				}
			}
		} else if (System.getProperty("PATH_IM").endsWith(File.separator)) System.getProperty("PATH_IM") else System.getProperty("PATH_IM") + File.separator

    /**
     * Setup IM4Java
     */
    ProcessStarter.setGlobalSearchPath(pathIM);

		// Check if "convert" is "ImageMagick convert" or some other app
		// If yes, check version
		val imCheck2 = new ProcessBuilder(pathIM + "convert", "--version").start();
		val IMCOPYRIGHT = convertStreamToString(imCheck2.getInputStream()) match {
			case Some(a) => a 	
      case None => {
				pUtil.printError("""Installed "convert" application is not part of the ImageMagick suite.""")
				pUtil.printIMNotFound
				println("Bye!")
				sys.exit(0)
				""
			}
		}
		imCheck2.waitFor()

    // FIXME This works in REPL and not here for some reason!
    /*val imCopyMatcher = """(?dm)Version\s*:\s*(ImageMagick\s+\d+.\S+).*$\s*Copyright\s*:\s*(.*)$\s*(.*)$""".r*/
    /*val CopyPrint = IMCOPYRIGHT match {*/
    /*  case imCopyMatcher(a,b,c) =>  a + "---" + b*/
    /*  case _ => "NONE"*/
    /*}*/

		if (IMCOPYRIGHT.indexOf("ImageMagick") < 0) {
			pUtil.printError("""Installed "convert" application is not part of the ImageMagick suite.""")
			pUtil.printIMNotFound
			println("Bye!")
			sys.exit(0)
		}

		println("using - " + IMCOPYRIGHT)

		if (args.length < 2) {
			// Not enough Program Arguments
			pUtil.printError("Try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [" + (if(args.length < 1)pUtil.clr(0, 31) else "") +
      "pathToSignatureDirectory"+ pUtil.clr + "] ["+ pUtil.clr(0, 31)+"pathToCrawl"+pUtil.clr+"]")
			println("Bye!")
      sys.exit(0)
		} else {
			// TODO check for directories
      val  targetDir = if(args(1).endsWith(File.separator)) args(1) else args(1) + File.separator;
      
      if(!(new File(args(0)).exists)) {
        pUtil.printError("""Target directory doesnot exist or is invalid.""")
        println("Bye!")
        sys.exit(0)
      }

      val sigd = new File(targetDir)
      val sigList = if(sigd.exists && sigd.isDirectory){
        sigd.list(signatureFilenameFilter).map(_.split("\\.")(0).toInt)
      }else {
        pUtil.printError("""Signature directory doesnot exist or is invalid.""")
        println("Bye!")
        sys.exit(0)
        // sys.exit should be safe till this point because 
        // we havn't started any threads or opened any streams yet.
      }

			// Ok now lets do our stuff
			try {
        /*
         * Prepare the ImageMagick operation to be performed
         * the out should be a 8bit gray map of the YUV channels of the source resized to 256 x 256 pixels
         * The channels are separated and appended vertically so that when the resulting stream is piped in,
         *    |------|
         *    |  Y   |  first 65536 bytes
         *    |------|  
         *    |  U   |  next 65536 bytes
         *    |------|  
         *    |  V   |  last 65536 bytes; a total of 196608 bytes.
         *    |------|
         */
        val cmd = new ConvertCmd();
        val op = new IMOperation();
        op.addImage();
        op.resize(256, 256, "!")
        op.colorspace("YUV")
        op.separate()
        op.appendVertically()
        op.size(256, 768)
        op.depth(8)
        op.addImage("GRAY:-");
        
        var fileCount = 0;
        pUtil.printStatus("crawl", "Crawling " + args(0) + " and Writing signatures to " + targetDir + "\n")
        walkDir(new File(args(0)))
        pUtil.printStatus("crawl", "Processing %d".format(fileCount) + " files.\n")

        /**
         * A fairly straight forward (depth first) fileSystem walker
         * TODO Implement detaled logging of types of files crawled.
         */
        def walkDir(dir: File): Unit = {
          for (f <- dir.listFiles(crawlerFileFilter)) {
            if (f.isDirectory()) 
              walkDir(f)
            else{
              // TODO Check if we have that file, if we have it check if the file has been modified
              if(!(sigList.exists(_ == f.hashCode) && WaveletSignatureFS.getSIgnatureDetailsGZ(targetDir + f.hashCode + ".sigz")._2 ==
                f.lastModified())){
                  // since we made the signature
                  val pr = new IMExecActor(cmd, op, targetDir)
                  pr.start
                  pr ! f
                  fileCount = fileCount + 1
                }
            }
            pUtil.printStatus("crawl", "%d".format(fileCount) + " files found.")
          }
        }
			} catch {
				case e: Exception => e.printStackTrace()
			}
		}
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
 * The workhorse. An actor that executes an IM4Java command and image operation, 
 * pipes the result in, makes a WaveletSignature [[holmes-wavelet.WaveletSignature]]
 * and saves the signature to the disc.
 */
class IMExecActor(cmd: ConvertCmd, op: IMOperation, targetDir:String) extends Actor {
  def act = {
    react {
      case f: File => try {
        val st = System.nanoTime()

        // Read the output of the IM command as a stream into memory
        val bos = new ByteArrayOutputStream(196608);
        val pipeOut:Pipe = new Pipe(null,bos);
        cmd.setOutputConsumer(pipeOut)
        cmd.run(op, f.getAbsolutePath())

        val buff = bos.toByteArray();
        bos.close()
        
        val iBuff = buff.map(_.toInt).toArray
        if(iBuff.length >= 196608){
          val sig = new WaveletSignature(iBuff, f.getAbsolutePath, f.lastModified);
          WaveletSignatureFS.saveToGZ(sig, targetDir + f.hashCode + ".sigz")
        }
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
  }
}

/**
 * FileFilter object used to filter filytypes during crawling
 */
object crawlerFileFilter extends FileFilter {
   /**
   * Suported file formats - by file extension
   * This is only a subset of fileformats Image magick supports
   */
  val fileTypes:Array[String] = Array(".ai", ".arw", ".bgr", ".bgra", ".bmp", ".bmp2", ".bmp3",
                                      ".cr2", ".crw", ".dcm", ".dcr", ".dcx", ".dng", ".dot",
                                      ".epdf", ".epi", ".eps", ".eps2", ".eps3", ".epsf", ".ept",
                                      ".efr", ".gif", ".gif87", ".icb", ".ico", ".icon", ".jng",
                                      ".jpeg", ".jpg", ".nef", ".nrw", ".orf", ".pcx", ".pdf",
                                      ".pix", ".png", ".png24", ".png32", ".psd", ".psb", ".sgi",
                                      ".sr2", ".srf", ".svg", ".svgz", ".tga", ".tiff", ".tiff64", ".tif",
                                      ".xcf")

  // FIXME Implementation is *imparative
  override def accept(f:File):Boolean = {
    if(f.isDirectory()) return true
    val n = f.getName().toLowerCase();
    for(ff <- fileTypes) if(n.endsWith(ff)) return true;
    return false;
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

