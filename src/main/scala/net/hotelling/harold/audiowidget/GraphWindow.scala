package net.hotelling.harold.audiowidget

import java.awt._
import javax.swing._

/**
  * Use Swing to display a graph in a window.
  */
class GraphWindow(val mixerNames: Array[String]) {
  @volatile var oscilloscope: Option[Graph] = None
  @volatile var intensity: Option[Graph] = None

  def display(): Unit = {
    SwingUtilities.invokeLater(DisplayDriver)
  }

  def getMaxYValueForOscilloscope: Option[Int] = oscilloscope.map { g => (g.getHeight - g.margin) / 2 }
  def getMaxYValueForFrequencies: Option[Int] = intensity.map { g => (g.getHeight - g.margin) / 2 }

  def setData(data: Array[Int]): Unit = setDataInOptionalGraph(oscilloscope, data)
  def setIntensity(data: Array[Int]): Unit = setDataInOptionalGraph(intensity, data)

  private def setDataInOptionalGraph(target: Option[Graph], data: Array[Int]): Unit = {
    target.foreach { g =>
      g.values = data
      g.repaint()
      g.revalidate()
    }
  }

  private object DisplayDriver extends Runnable {
    override def run(): Unit = {
      val frame = new JFrame("Audio Widget")
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

      val mixerName = new JComboBox[String](mixerNames)

      val graph = new Graph(400)
      GraphWindow.this.oscilloscope = Some(graph)

      val keyboard = new Keyboard()

      GraphWindow.this.intensity = Some(new Graph(200))

      val container = new JPanel()
      container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS))

      container.add(mixerName)
      container.add(graph)
      container.add(keyboard)
      container.add(GraphWindow.this.intensity.get)

      frame.getContentPane.add(container)
      frame.pack()
      frame.setLocationByPlatform(true)
      frame.setVisible(true)
    }
  }
}

class Graph(val preferredHeight: Int = 700) extends JPanel {
  @volatile var values: Array[Int] = (1 to 1024).map(i => (100f * Math.sin(i / 20f)).toInt).toArray
  val margin: Int = 20
  val axesColor = new Color(128, 128, 255)
  val dataColor = new Color(255, 128, 128)
  val backgroundColor = new Color(255, 255, 255)

  override def getPreferredSize = new Dimension(1024, preferredHeight)

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g2d.setColor(backgroundColor)
    g2d.fillRect(0, 0, getWidth, getHeight)

    val width = getWidth
    val height = getHeight
    val baseY = height/2

    g2d.setColor(axesColor)
    g2d.drawLine(margin, margin, margin, height - margin)
    g2d.drawLine(margin, baseY, width - margin, baseY)

    g2d.setColor(dataColor)
    (1 until Math.min(values.length, width - (2 * margin))).foreach { i =>
      val x1 = margin + i - 1
      val x2 = margin + i
      val y1 = baseY - values(i-1)
      val y2 = baseY - values(i)
      g2d.drawLine(x1, y1, x2, y2)
    }
  }
}

class Keyboard extends JPanel {
  val preferredWidth = 1024
  val numOctaves = 5
  val numKeys = 7 * numOctaves
  val keywidth = (preferredWidth.toFloat / numKeys).toInt

  val white = new Color(255, 255, 255)
  val black = new Color(0, 0, 0)

  override def getPreferredSize = new Dimension(preferredWidth, 100)

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setColor(white)
    g2d.fillRect(0, 0, getWidth, getHeight)
    g2d.setColor(black)
    (0 to numKeys) foreach { i =>
      drawWhiteKey(g2d, i)
      if (isBlackNote(i)) drawBlackKey(g2d, i)
    }
  }

  def drawWhiteKey(g2d: Graphics2D, i: Int): Unit = {
    g2d.drawRect(keywidth * i + 1, 1, keywidth - 1, getHeight - 1)
  }

  def drawBlackKey(g2d: Graphics2D, i: Int): Unit = {
    val x1 = (keywidth * (i - 0.25)).toInt
    val height = (getHeight * 0.4).toInt
    g2d.fillRect(x1, 0, keywidth / 2, height)
  }


  val blackNotes = Set(1, 2, 4, 5, 6)

  def isBlackNote(i: Int) = blackNotes.contains(i % 7)
}
