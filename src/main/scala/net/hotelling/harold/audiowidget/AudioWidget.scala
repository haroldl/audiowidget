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

        val intensityData = DFT.dft(data, parser.format.getFrameRate, 40, 5000).map { v => (v * 10).toInt }
        window.setIntensity(rescaleData(intensityData, window.getMaxYValueForFrequencies.getOrElse(1000).toFloat))

        val zS = zScores(intensityData)
        window.setZScores(zS.map { _ * window.getMaxYValueForZScores.getOrElse(300).toFloat }.map { _.toInt })
      } else {
        println("Empty audio input read.")
      }
    }
  }

  def rescaleData(data: Array[Int], desiredMaxAmplitude: Float): Array[Int] = {
    val (min, max) = (data.min, data.max)
    val amplitude = Math.max(Math.abs(min), Math.abs(max))
    val scale = desiredMaxAmplitude / amplitude.toFloat
    data.map(_ * scale).map(_.toInt)
  }

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
