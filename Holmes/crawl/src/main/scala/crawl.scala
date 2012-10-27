
import java.io.BufferedReader
import scala.actors.Actor
import scala.actors.Actor._
import java.awt.image.BufferedImage
import java.io.InputStreamReader
import java.io.InputStream
import scala.io.Source
import java.io.File
import java.io.FileFilter
import org.im4java.process
import org.im4java.process._
import org.im4java.process.ProcessStarter
import org.im4java.core
import org.im4java.core._

object crawl{

	val MAC = 1
	val WIN = 2
	val NIX = 3
	val NUL = -1

	var CONSOLE = 0
  
  val crawlerFileFilter = new CrawlerFileFilter
	
	def main(args: Array[String]): Unit = {
		val OSNAME = System.getProperty("os.name")
		val OSVERSION = System.getProperty("os.version")

		println("Holmes Crawler version 1.0")
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
		
		// Try to find ImageMagick
		val imCheck = new ProcessBuilder("which", "convert").start();
		val imCheckOut = convertStreamToString(imCheck.getInputStream())
		imCheck.waitFor()

		val pathIM = if (System.getProperty("PATH_IM") == null) {
			val matchstr = File.separator + "convert"
			val whichstr = ("""(.*)(""" + matchstr + ")").r;
			imCheckOut match {
				case Some(a) => a match {
					case whichstr(a, matchstr) => a + File.separator
					case _ => {
						printIMNotFound
						sys.exit(2)
						""
					}
				}
				case None => {
					printIMNotFound
					sys.exit(2)
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
				printError("""Installed "convert" application is not part of the ImageMagick suite.""")
				printIMNotFound
				println("Bye!")
				sys.exit(2)
				""
			}
		}
		imCheck2.waitFor()

  // TODO use scala's regex pattern matching on case classes to do this job, above...
		if (IMCOPYRIGHT.indexOf("ImageMagick") < 0) {
			printError("""Installed "convert" application is not part of the ImageMagick suite.""")
			printIMNotFound
			println("Bye!")
			sys.exit(2)
			// sys.exit should be safe till this point because 
			// we havn't started any threads or opened any streams yet.
		}

		println("With help from\n" + IMCOPYRIGHT)

		if (args.length < 2) {
			// Not enough Program Arguments
			printError("Try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [" + (if(args.length < 1)clr(0, 31) else "") +
      "pathToSignatureDirectory"+  clr + "] ["+ clr(0, 31)+"pathToCrawl"+clr+"]")
			println("Bye!")
		} else {
			// TODO check for directories

			// Ok now lets do our stuff
			try {
				// FIXME 'stream' returns a bunch of '0's in Windows 7! Use bufferedImage for reliability!
				// this is so far tested, successfully, only on mac.
        
        val cmd = new ConvertCmd();
        val op = new IMOperation();
        op.addImage();
        op.resize(256, 256, "!")
        op.colorspace("YUV")
        op.separate()
        op.appendVertically()
        op.addImage();

        /**
         * A fairly straight forward fileSystem walker
         */
        def walkDir(dir: File): Unit = {
          for (f <- dir.listFiles()) {
            val v = f.getPath().toLowerCase()
            // TODO Use a filefilter!
            if (v.endsWith(".jpg") || v.endsWith(".jpeg")) {
              val pr = new IMExecActor(cmd, op)
              pr.start
              pr ! f
              /*fileCount = fileCount + 1;*/
            }
           if (f.isDirectory()) walkDir(f);
          }
        }
			} catch {
				case e: Exception => e.printStackTrace()
			}
		}
	}

	def printIMNotFound = {
		printError("""Crawler needs ImageMagick to run. Install ImageMagick (http://www.imagemagick.org)""")
		printWarn("Then try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [pathToSignatureDirectory] [pathToCrawl]")
	}

  def printError(str:String) {
		println("[" + clr(0, 31) + "Error" + clr + "]: " + str)
  }
  def printWarn(str:String) {
		println("[" + clr(0, 33) + "Warning" + clr + "]: " + str)
  }

	// ANSI coloured output on supported terminals
	def clr(a: Int, b: Int) = {
		// "\033[0;36m" for coloured text. 36-cyan and so on
		if (CONSOLE > 0) "\033[" + a.toString + ";" + b.toString + "m" else ""
	}

	def clr = {
		// "\033[0m" for normal colours
		if (CONSOLE > 0) "\033[0m" else ""
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


class IMExecActor(cmd: ConvertCmd, op: IMOperation) extends Actor {
  val targetDir = "/Users/hari/Documents/Work/signatures/"
  // TODO implement piping in to bufferedImage
  def act = {
    react {
      case f: File => try {
        cmd.run(op, f.getAbsolutePath(), targetDir + f.getName() + ".png");
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
  }
}

class CrawlerFileFilter extends FileFilter {
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

  // Implementation is *imparative
  override def accept(f:File):Boolean = {
    val n = f.getName().toLowerCase();
    for(ff <- fileTypes) if(n.endsWith(ff)) return true;
    return false;
  }
}

