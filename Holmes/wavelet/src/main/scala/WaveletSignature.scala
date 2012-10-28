
import scala.math._
import scala.collection.mutable.ArrayBuffer
import WaveLetSignatureConstants._

class WaveletSignature(imgyui: Array[Int], fname: String, lastmod: Long) extends Serializable {
	var fileName: String = fname
	var lastModified: Long = lastmod

	val Y = new ArrayBuffer[Int]()
	val U = new ArrayBuffer[Int]()
	val V = new ArrayBuffer[Int]()
	val Ys = new ArrayBuffer[Int]()
	val Us = new ArrayBuffer[Int]()
	val Vs = new ArrayBuffer[Int]()
	var sigY = 0.0
	var sigU = 0.0
	var sigV = 0.0

	// Initialized only for queries
	var sigYb = 0.0
	var sigUb = 0.0
	var sigVb = 0.0
	var sigYdivb = 0.0
	var sigUdivb = 0.0
	var sigVdivb = 0.0

	if (imgyui != null) {
		val sizesq = SIZE * SIZE;

		var t: DWT_CDF_9_7 = new DWT_CDF_9_7(SIZE, SIZE, LEVEL1)
		reduce(t.forward(imgyui.take(sizesq).toArray, 0), S1, SIZE, Y)
		reduce(t.forward(imgyui.drop(sizesq).take(sizesq).toArray, 0), S1, SIZE, U)
		reduce(t.forward(imgyui.takeRight(sizesq).toArray, 0), S1, SIZE, V)

		t = new DWT_CDF_9_7(SIZE, SIZE, LEVEL2)
		reduce(t.forward(imgyui.take(sizesq).toArray, 0), S2, SIZE, Ys)
		reduce(t.forward(imgyui.drop(sizesq).take(sizesq).toArray, 0), S2, SIZE, Us)
		reduce(t.forward(imgyui.takeRight(sizesq).toArray, 0), S2, SIZE, Vs)

		sigY = featureSD(reduce(Y, S1 >> 2, S1))
		sigU = featureSD(reduce(U, S1 >> 2, S1))
		sigV = featureSD(reduce(V, S1 >> 2, S1))
	}

	def this() = this(null, "", 0)
	def this(imgyui: Array[Int]) = this(imgyui, "", 0)
	def this(fname: String, lastmod: Long) = this(null, fname, lastmod)

	def InitSignature(percent: Int) = {
		val beta: Double = 1.0 - percent / 100.0
		sigYb = sigY * beta
		sigUb = sigU * beta
		sigVb = sigV * beta
		sigYdivb = sigY / beta
		sigUdivb = sigU / beta
		sigVdivb = sigV / beta
	}

	private def reduce(y2: ArrayBuffer[Int], stride: Int, width: Int) = {
		val ep1 = (stride - 1) * width
		val ret = new Array[Int](stride * stride)
		var cnt = 0

		for (i <- 0 to ep1 by width)
			for (j <- i until (i + stride)) {
				ret(cnt) = y2(j)
				cnt += 1
			}
		ret
	}

	private def reduce(y2: Array[Int], stride: Int, width: Int, dst: ArrayBuffer[Int]) = {
		val ep1 = (stride - 1) * width

		for (i <- 0 to ep1 by width)
			for (j <- i until (i + stride))
				dst += y2(j)
	}

	private def featureSD(y2: Array[Int]): Double = {
		val mean = y2.reduceLeft(_ + _) / y2.length.toDouble
		def sqDiff(v1: Double, v2: Double): Double = pow(v1 - v2, 2.0)
		val ret = y2.foldLeft(0.0)(_ + sqDiff(_, mean))
		sqrt(ret / y2.length.toDouble)
	}

}
