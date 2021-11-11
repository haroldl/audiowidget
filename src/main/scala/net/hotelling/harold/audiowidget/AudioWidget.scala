package net.hotelling.harold.audiowidget


object AudioWidget {

  def main(args: Array[String]) {
    val inputMixerNames = Audio.mixerNamesWithInputSupport()
    val window = new GraphWindow(inputMixerNames)
    window.display()

    val defaultMixerName = "default [default]"
    val mixerName = if (inputMixerNames.contains(defaultMixerName)) defaultMixerName else inputMixerNames(0)
    val mixer = Audio.getMixerByName(mixerName)
    val line = Audio.getSomeInputLine(mixer)
    val parser = new Audio(line.getFormat)
    while (true) {
      val data = parser.readFrames(line)
      if (!data.isEmpty) {
        window.setData(rescaleData(data, window.getMaxYValueForOscilloscope.getOrElse(1000).toFloat))

        val intensityData = DFT.dft(data, parser.format.getFrameRate, 40, 5000, 15)
          .map { v => (v * 10).toInt }
        window.setIntensity(smooth(rescaleData(intensityData, window.getMaxYValueForFrequencies.getOrElse(1000).toFloat)))

        val indexOfMaxIntensity = intensityData.zipWithIndex.maxBy(_._1)._2
        val frequencyWithMaxIntensity = 40 + (15 * indexOfMaxIntensity)
        window.setStrongestFrequencyHz(frequencyWithMaxIntensity)

        val zS = zScores(intensityData)
        window.setZScores(zS.map { _ * window.getMaxYValueForZScores.getOrElse(300).toFloat }.map { _.toInt })
      } else {
        println("Empty audio input read.")
      }
    }
  }

  /** Rescaling the data adjusts the values so that the largest values match the
    * desiredMaxAmplitude. This effectively zooms in/out on the data so that we
    * can visualize it nicely.
    */
  def rescaleData(data: Array[Int], desiredMaxAmplitude: Float): Array[Int] = {
    val (min, max) = (data.min, data.max)
    val amplitude = Math.max(Math.abs(min), Math.abs(max))
    val scale = desiredMaxAmplitude / amplitude.toFloat
    data.map(_ * scale).map(_.toInt)
  }

  /** The simplest smoothing algorithm: replace each value with the average of the
    * m data points centered on that point, where m is usually required to be odd.
    */
  def smooth(data: Array[Int], m: Int = 3): Array[Int] = {
    (0 to data.length).map(idx => {
      val valuesToAverage = (-m / 2 to m / 2).map(offset =>
        data(Math.max(0, Math.min(data.length - 1, idx + offset))))
      valuesToAverage.sum / m
    }).toArray
  }

  /** Z-scores in statistics just means representing each value as how many
    * standard deviations away from the mean it is. One effect is that data with
    * different means but varying in the same way will have the same Z-scores.
    */
  def zScores(data: Array[Int]): Array[Double] = {
    val mean: Double = data.sum.toDouble / data.length
    val variance = data.map { v =>
      val deviation = mean - v
      deviation * deviation
    }.sum
    val standardDeviation = Math.sqrt(variance)
    data.map { v => (v - mean) / standardDeviation }
  }
}
