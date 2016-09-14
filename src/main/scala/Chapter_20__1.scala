import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

//import akka.actor.{Actor, ActorSystem, Props}
//import akka.routing.RoundRobinRouter

object Chapter_20__1 extends App {

  val img = ImageIO.read(getClass.getResourceAsStream("img.jpg"))
  val res = new BufferedImage(img.getWidth, img.getHeight, img.getType)
  val start = System.currentTimeMillis
  for (i <- 0 until img.getHeight; j <- 0 until img.getWidth) res.setRGB(j, i, 0xffffff ^ img.getRGB(j, i))
  println(System.currentTimeMillis - start)
  ImageIO.write(res, "jpg", new File("C:\\scala-out\\img-invert.jpg"))

//  sealed trait Message
//
//  case class Invert(source: BufferedImage, target: BufferedImage, offset: Int, step: Int) extends Message
//
//  case object Inverted extends Message

  //  class Worker extends Actor {
  //    override def receive = {
  //      case Invert(source, target, offset, step) => {
  //        for (x <- 0 until target.getWidth; y <- offset until target.getHeight by step)
  //          target.setRGB(x, y, 0xffffff ^ source.getRGB(x, y))
  //        sender ! Inverted
  //      }
  //    }
  //  }
  //
  //  class Master(sourceImageName: String, val targetImageFullName: String, numberOfWorkers: Int) extends Actor {
  //
  //    val sourceImage = ImageIO.read(getClass.getResourceAsStream(sourceImageName))
  //    val targetImage = new BufferedImage(sourceImage.getWidth, sourceImage.getHeight, sourceImage.getType)
  //
  //    val workersPool = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(numberOfWorkers)))
  //
  //    for (i <- 0 until numberOfWorkers)
  //      workersPool ! Invert(sourceImage, targetImage, i, numberOfWorkers)
  //
  //    val startMillis = System.currentTimeMillis
  //
  //    private var finishedWorkers = 0
  //
  //    override def receive = {
  //      case Inverted => {
  //        finishedWorkers += 1
  //        if (finishedWorkers == numberOfWorkers) {
  //          println("Duration millis = " + (System.currentTimeMillis - startMillis))
  //          ImageIO.write(targetImage, "jpg", new File(targetImageFullName))
  //          context.stop(self)
  //        }
  //      }
  //    }
  //  }
  //
  //  ActorSystem().actorOf(Props(new Master("img.jpg", "C:\\scala-out\\img-invert.jpg", 1)))
}
