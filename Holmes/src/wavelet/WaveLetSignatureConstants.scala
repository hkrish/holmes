package wavelet

object WaveLetSignatureConstants {
	val SIZE: Int = 256
	val LEVEL1: Int = 4
	val LEVEL2: Int = 5

	val S1 = SIZE >> (LEVEL1 - 1)
	val S2 = SIZE >> (LEVEL2 - 1)
}