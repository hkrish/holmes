import java.io.File
import scala.actors.Actor
import scala.actors.Actor._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.io.Source
import org.im4java.process
import org.im4java.process._
import org.im4java.process.ProcessStarter
import org.im4java.core
import org.im4java.core._

object Walker {
  var Start = new File("""/Volumes/HK's passport/My pictures/Bikes""")

  val pathIM = "/usr/local/bin/"
  val myPath = pathIM;
  ProcessStarter.setGlobalSearchPath(myPath);

  val cmd = new ConvertCmd();
  val op = new IMOperation();
  op.addImage();
  op.resize(256, 256, "!")
  op.colorspace("YUV")
  op.separate()
  op.appendVertically()
  op.addImage();

  var fileCount = 0;

  //	var Start = new File(System.getProperty("user.home"))

  //	val pr = actor {
  //		loop {
  //			react {
  //				case (s: String) => println(s);
  //				case (f: File) => println(f.getPath());
  //				case i: Int => if (i == 0) exit();
  //			}
  //		}
  //	}
  //	println(System.getProperty("actors.maxPoolSize"))

  def walkDir(dir: File): Unit = {
    for (f <- dir.listFiles()) {
      val v = f.getPath().toLowerCase()
      if (v.endsWith(".jpg") || v.endsWith(".jpeg")) {
        val pr = new JPGprinter(cmd, op)
        pr.start
        pr ! f
        fileCount = fileCount + 1;
      }
     if (f.isDirectory()) walkDir(f);
    }
  }

  walkDir(Start)
}

class JPGprinter(cmd: ConvertCmd, op: IMOperation) extends Actor {
  val targetDir = "/Users/hari/Documents/Work/signatures/"

  def act = {
    react {
      case f: File => try {
        cmd.run(op, f.getAbsolutePath(), targetDir + f.getName() + ".png");
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

////object Walker extends App {
////	var Start = new File("""/Volumes/HK's passport/My pictures/Bikes""")
////	
////	//	var Start = new File(System.getProperty("user.home"))
////	
////	//	val pr = actor {
////	//		loop {
////	//			react {
////	//				case (s: String) => println(s);
////	//				case (f: File) => println(f.getPath());
////	//				case i: Int => if (i == 0) exit();
////	//			}
////	//		}
////	//	}
////	
////	//	println(System.getProperty("actors.maxPoolSize"))
////	
////	def walkDir(dir: File): Unit = {
////		for (f <- dir.listFiles()) {
////			val v = f.getPath().toLowerCase()
////					if (v.endsWith(".jpg") || v.endsWith(".jpeg")) {
////						val pr = new JPGprinter
////								pr.start
////								pr ! f
////					}
////			if (f.isDirectory()) walkDir(f);
////		}
////	}
////	
////	walkDir(Start)
////}
////
////class JPGprinter extends Actor {
////	val pathIM = "/usr/local/bin/"
////			val targetDir = "/Users/hari/Documents/Work/signatures/"
////			
////			def act = {
////			react {
////			case f: File => try {
////				//convert -quiet '/Volumes/HK's passport/My pictures/Bikes/Bikes/duc_007_2005_00_1024x768_ducati-1000.jpg' -resize 256x256! -colorspace YUV -separate -append  PNG:out.png
////				//				val cmd1 = pathIM + "convert -quiet \"" + f.getAbsolutePath() + "\" -resize 256x256! -colorspace YUV -separate -append  PNG:-"
////				
////				val fn = f.getAbsolutePath().replaceAll("\\s", "\\\\ ").replaceAll("\'", "\\\\'")
////						
//////				val cmd1 = new ProcessBuilder("./sampleApp", "convert " + fn + " -resize 256x256! -colorspace YUV -separate -append  PNG:" + targetDir + f.getName() + ".png")
////						val cmd1 = new ProcessBuilder("./sampleApp", "convert " + fn + " -resize 256x256! -colorspace YUV -separate -append  PNG:-")
////				val pr1 = cmd1.start();
////				
////				//				val cmd1 = pathIM + "convert " +f.getAbsolutePath() + " -resize 256x256! -colorspace YUV -separate -append  PNG:-"
////				//				val rt = Runtime.getRuntime()
////				//				val pr1 = rt.exec(cmd1)
////				
////				val is = pr1.getInputStream()
////						val bytes = Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray
//////        println(convertStreamToString(pr1.getErrorStream()))
////						val exitVal1 = pr1.waitFor()
////						
////						if (exitVal1 == 0) {
////							val img = ImageIO.read(new ByteArrayInputStream(bytes));
////							ImageIO.write(img, "png", new File(targetDir + f.getName() + ".png"))
////						}
////				
////			} catch {
////			case e: Exception => e.printStackTrace()
////			}
////			}
////	}
////	
////	def convertStreamToString(is: InputStream): Option[String] = try {
////		val src = Source.fromInputStream(is).getLines
////				if (src.isEmpty) None else Some(src.reduceLeft(_ + "\n" + _))
////	} catch {
////	case e: Exception => {
////		e.printStackTrace()
////		None
////	}
////	}
////}
