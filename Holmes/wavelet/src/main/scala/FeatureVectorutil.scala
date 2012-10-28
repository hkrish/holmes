
import scala.collection.mutable.ArrayBuffer
import WaveLetSignatureConstants._

object FeatureVectorutil {
	def checkSigmaother(query: WaveletSignature, other: WaveletSignature): Boolean = {
		(query.sigYb < other.sigY && other.sigY < query.sigYdivb) || ((query.sigUb < other.sigU && other.sigU < query.sigUdivb) && (query.sigVb < other.sigV && other.sigV < query.sigVdivb))
	}

	def getFirstMatrixDistanceWith(query: WaveletSignature, other: WaveletSignature): Double = {
		euclidianDist(query.Ys, other.Ys, S1) + euclidianDist(query.Us, other.Us, S1) + euclidianDist(query.Vs, other.Vs, S1);
	}

	def getDistanceWith(query: WaveletSignature, other: WaveletSignature): Double = {
		euclidianDist(query.Y, other.Y, S1) + euclidianDist(query.U, other.U, S1) + euclidianDist(query.V, other.V, S1);
	}

	/**
	 * @param to
	 * @return The computer Euclidean Distance between two matrices
	 */
	private def euclidianDist(m1: ArrayBuffer[Int], m2: ArrayBuffer[Int], stride: Int): Double = {
		var ret: Double = 0.0
		val SQ = stride * stride
		var i = 0

		for (i <- 0 until SQ) {
			ret += (m1(i) - m2(i)) * (m1(i) - m2(i));
		}

		Math.sqrt(ret);
	}
}
