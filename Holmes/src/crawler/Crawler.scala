package crawler

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream
import scala.io.Source
import java.io.File

object Crawler {

	val MAC = 1
	val WIN = 2
	val NIX = 3
	val NUL = -1

	//	var COLPRE = ""
	//	var COLSUF = ""
	var CONSOLE = 0

	def main(args: Array[String]): Unit = {
		val OSNAME = System.getProperty("os.name")
		val OSVERSION = System.getProperty("os.version")

		//		println("\033[0;36mbold red text\033[0m\n")

		// TODO check os version as well
		// Mac > 10.5? - test
		// Win > XP? - test
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

		println("Holmes Crawler version 1.0")
		println(OSNAME + " " + OSVERSION)

		if (OS == NUL) {
			println("Unsupported OS")
			println("Bye!")
		}

		// Try to find ImageMagick
		val pb = new ProcessBuilder("which", "convert");
		val imCheck = pb.start();
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

		//		println("pathIM=" + pathIM)

		// Check if "convert" is "ImageMagick convert" or some other app
		// If yes, check version
		val pb2 = new ProcessBuilder(pathIM + "convert", "--version");
		val imCheck2 = pb2.start();
		val IMCOPYRIGHT = convertStreamToString(imCheck2.getInputStream(), "    ") match {
			case Some(a) => a
			case None => {
				println("""Installed "convert" application is not part of the ImageMagick suite.""")
				printIMNotFound
				println("Bye!")
				sys.exit(2)
				""
			}
		}
		imCheck2.waitFor()

		if (IMCOPYRIGHT.indexOf("ImageMagick") < 0) {
			println("""Installed "convert" application is not part of the ImageMagick suite.""")
			printIMNotFound
			println("Bye!")
			sys.exit(2)
			// sys.exit should be safe till this point because 
			// we havn't started any threads or opened any streams yet.
		}

		println("With help from\n" + IMCOPYRIGHT)

		if (args.length < 2) {
			// Not enough Program Arguments
			println(clr(0, 31) + "Try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [pathToSignatureDirectory] [pathToCrawl]" + clr)
			println("Bye!")
		} else {

			try {
				val cmd1 = pathIM + "convert -quiet /Users/hari/Downloads/svg_examples/penguin.svg -resize 256x256! -colorspace YUV -separate -append -define png:color-type=2  PNG:-"
				val cmd2 = pathIM + "stream -map i -storage-type char - -"

				// FIXME USe ProcessBuilder
				val rt = Runtime.getRuntime()
				val pr1 = rt.exec(cmd1)

				val timer = System.nanoTime()

				val is = pr1.getInputStream()
				val bytes = Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray

				val exitVal1 = pr1.waitFor()
				System.out.println("Exited with error code " + exitVal1);

				if (exitVal1 == 0) {
					val pr2 = rt.exec(cmd2)
					val os2 = pr2.getOutputStream()
					val is2 = pr2.getInputStream()
					os2.write(bytes)
					os2.flush()
					os2.close()
					val ints = Stream.continually(is2.read).takeWhile(-1 !=).map(_.toInt).toList
					println(ints.length)

					println(convertStreamToString(pr2.getErrorStream()))

					val exitVal2 = pr2.waitFor()
					System.out.println("Exited with error code " + exitVal2)
				}

				println("Took " + (System.nanoTime() - timer) / 1000000 + " ms")

			} catch {
				case e: Exception => e.printStackTrace()
			}
		}
	}

	def printIMNotFound = {
		println(clr(0, 31) + """Crawler needs ImageMagick to run. Install ImageMagick (http://www.imagemagick.org)""")
		println("Then try: scala -DPATH_IM=[pathToImageMagick] crawler.IMCrawler [pathToSignatureDirectory] [pathToCrawl]" + clr)
	}

	// ANSI colored output on supported terminals
	def clr(a: Int, b: Int) = {
		// "\033[0;36m"
		if (CONSOLE > 0) "\033[" + a.toString + ";" + b.toString + "m" else ""
	}

	def clr = {
		// "\033[0m"
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

	def convertStreamToString(is: InputStream, prefix: String): Option[String] = try {
		val src = Source.fromInputStream(is).getLines
		if (src.isEmpty) None else Some(prefix + src.reduceLeft(_ + "\n" + prefix + _))
	} catch {
		case e: Exception => {
			e.printStackTrace()
			None
		}
	}
}



//package crawler
//
//import java.io.BufferedReader
//import java.io.InputStreamReader
//import java.io.InputStream
//import scala.io.Source
//import java.io.File
//
//object IMCrawler {
//
//	def main(args: Array[String]): Unit = {
//		if (args.length < 1) {
//			println("IMCrawler needs ImageMagick to run");
//			println("Usage:");
//			println("\tscala crawler.IMCrawler [pathToImageMagick]");
//		} else
//			try {
//				val rt = Runtime.getRuntime();
//				val pathIM = if (args(0).endsWith(File.separator)) args(0) else args(0) + File.separator
//				val env: Array[String] = Array("PATH=" + System.getenv("PATH") + File.pathSeparator + pathIM)
//
//				val tmpf = File.createTempFile("tempf", ".tmp")
//				tmpf.deleteOnExit();
//
//				val cmd1 = pathIM + "convert -quiet /Users/hari/Downloads/svg_examples/penguin.svg -resize 256x256! -colorspace YUV -separate -append -define png:color-type=2  PNG:" + tmpf.getAbsolutePath();
//				val cmd2 = pathIM + "stream -map i -storage-type char " + tmpf.getAbsolutePath() + " -";
//
//				val timer = System.nanoTime();
//				
//				val pr1 = rt.exec(cmd1, env);
//				println(convertStreamToString(pr1.getInputStream()))
//				println(convertStreamToString(pr1.getErrorStream()))
//				val exitVal1 = pr1.waitFor();
//				System.out.println("Exited with error code " + exitVal1);
//
//				if (exitVal1 == 0) {
//					val pr2 = rt.exec(cmd2, env);
//					val os2 = pr2.getOutputStream();
//
//					val is2 = pr2.getInputStream();
//					val bytes = Stream.continually(is2.read).takeWhile(-1 !=).map(_.toByte).toArray
//					println(bytes.length)
//
//					println(convertStreamToString(pr2.getErrorStream()))
//
//					val exitVal2 = pr2.waitFor();
//					System.out.println("Exited with error code " + exitVal2);
//
//					tmpf.delete();
//				}
//				println("Took " + (System.nanoTime() - timer) / 1000000 + " ms")
//
//			} catch {
//				case e: Exception => {
//					System.out.println(e.toString());
//					e.printStackTrace();
//				}
//			}
//
//		def convertStreamToString(is: InputStream): String = try {
//			val src = Source.fromInputStream(is).getLines
//			if (src.length > 0) src.reduceLeft(_ + "\n" + _) else ""
//		} catch {
//			case e: Exception => e.toString();
//		}
//	}
//
//}
