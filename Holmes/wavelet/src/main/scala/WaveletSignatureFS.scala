
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileNotFoundException
import java.io.IOException
import WaveLetSignatureConstants._

object WaveletSignatureFS {
	def saveTo(sig: WaveletSignature, fname: String) = {
		try {
			val fos = new FileOutputStream(fname)
			val oos = new ObjectOutputStream(fos)

			oos.writeUTF(sig.fileName)
			oos.writeLong(sig.lastModified);

			sig.Y.foreach(oos.writeInt(_))
			sig.U.foreach(oos.writeInt(_))
			sig.V.foreach(oos.writeInt(_))

			sig.Ys.foreach(oos.writeInt(_))
			sig.Us.foreach(oos.writeInt(_))
			sig.Vs.foreach(oos.writeInt(_))

			oos.writeDouble(sig.sigY);
			oos.writeDouble(sig.sigU);
			oos.writeDouble(sig.sigV);

			oos.close();
		} catch {
			case e1: FileNotFoundException => {
				System.err.println("Error saving signature to  " + fname);
				e1.printStackTrace();
			}
			case e1: IOException => {
				System.err.println("Error saving signature to  " + fname);
				e1.printStackTrace();
			}
		}
	}

	def loadFrom(fname: String): WaveletSignature = {
		var ret: WaveletSignature = null;
		try {
			val fis = new FileInputStream(fname);
			val ois = new ObjectInputStream(fis);

			val fileName = ois.readUTF();
			val lastModified = ois.readLong();

			ret = new WaveletSignature(fileName, lastModified)

			var i = 0
			val s1q = S1 * S1
			val s2q = S2 * S2

			for (i <- 0 until s1q)
				ret.Y += ois.readInt()
			for (i <- 0 until s1q)
				ret.U += ois.readInt()
			for (i <- 0 until s1q)
				ret.V += ois.readInt()

			for (i <- 0 until s2q)
				ret.Ys += ois.readInt()
			for (i <- 0 until s2q)
				ret.Us += ois.readInt()
			for (i <- 0 until s2q)
				ret.Vs += ois.readInt()

			ret.sigY = ois.readDouble();
			ret.sigU = ois.readDouble();
			ret.sigV = ois.readDouble();

			ois.close();
		} catch {
			case e1: FileNotFoundException => {
				System.err.println("Error saving signature to  " + fname);
				e1.printStackTrace();
			}
			case e1: IOException => {
				System.err.println("Error saving signature to  " + fname);
				e1.printStackTrace();
			}
		}

		ret;
	}
}