package crawler

import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream
import scala.io.Source
import java.io.File

object IMCrawler {

	def main(args: Array[String]): Unit = {

		if (args.length < 3) {
			println("Crawler needs ImageMagick to run")
			println("Usage:")
			println("\tscala crawler.IMCrawler [pathToImageMagick] [pathToSignatureDirectory] [pathToCrawl]")
		} else {
			val OS = System.getProperty("os.name");

			println("Holmes Crawler version 1.0");
			println(OS);

			try {
				val rt = Runtime.getRuntime();
				val pathIM = if (args(0).endsWith(File.separator)) args(0) else args(0) + File.separator
				val env: Array[String] = Array("PATH=" + System.getenv("PATH") + File.pathSeparator + pathIM)

				val cmd1 = pathIM + "convert -quiet /Users/hari/Downloads/svg_examples/penguin.svg -resize 256x256! -colorspace YUV -separate -append -define png:color-type=2  PNG:-"
				val cmd2 = pathIM + "stream -map i -storage-type char - -"

				val pr1 = rt.exec(cmd1, env)

				val timer = System.nanoTime()

				val is = pr1.getInputStream()
				val bytes = Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray

				val exitVal1 = pr1.waitFor();
				System.out.println("Exited with error code " + exitVal1);

				if (exitVal1 == 0) {
					val pr2 = rt.exec(cmd2, env)
					val os2 = pr2.getOutputStream()
					val is2 = pr2.getInputStream()
					os2.write(bytes);
					os2.flush();
					os2.close();
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

		def convertStreamToString(is: InputStream): String = try {
			val src = Source.fromInputStream(is).getLines
			if (src.length > 0) src.reduceLeft(_ + "\n" + _) else ""
		} catch {
			case e: Exception => {
				e.printStackTrace()
				e.toString()
			}
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
