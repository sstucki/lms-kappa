package kappa

import org.scalatest.FlatSpec

import scala.math._
import breeze.linalg._

class TestKaSpaceModel extends KaSpaceModel with FlatSpec
{
  type Pos = DenseVector[Double]
  type Orientation = DenseMatrix[Double]

  def Rx(tetha: Double): Orientation = new DenseMatrix(3, 3, Array(
    1,          0,           0,
    0, cos(tetha), -sin(tetha),
    0, sin(tetha),  cos(tetha)))

  def Ry(tetha: Double): Orientation = new DenseMatrix(3, 3, Array(
     cos(tetha), 0, sin(tetha),
              0, 1,          0,
    -sin(tetha), 0, cos(tetha)))

  def Rz(tetha: Double): Orientation = new DenseMatrix(3, 3, Array(
    cos(tetha), -sin(tetha), 0,
    sin(tetha),  cos(tetha), 0,
             0,           0, 1))

  // Agent radius
  val radius: Double = 1

  // Site positions
  val posL : Pos = DenseVector(-cos(Pi/7), sin(Pi/7), 0)
  val posR : Pos = DenseVector( cos(Pi/7), sin(Pi/7), 0)
  val posBL: Pos = DenseVector(-1/2, 1/2 * tan(Pi/14), -1)
  val posBR: Pos = DenseVector( 1/2, 1/2 * tan(Pi/14), -1)

  // Link orientations
  val wLR: Orientation = Rz(-2 * Pi / 7)
  val wRL: Orientation = Rz( 2 * Pi / 7)
  val wBL: Orientation = Rz(-Pi / 7) * Ry(Pi)
  val wBR: Orientation = Rz( Pi / 7) * Ry(Pi)

  // Contact graph
  contactGraph = k"""A:{$radius}(l:{$posL}!{1},
                                 r:{$posR}!{1},
                                 bl:{$posBL}!{2,2},
                                 br:{$posBR}!{3,3}),
                     1:{$wLR.$wRL},
                     2:{$wBL.$wBL},
                     3:{$wBR.$wBR}"""

  // Rules
  val bindLR = "A(r) , A(l) " -> "A(r!1) , A(l!1) " :@ 1
  val bindBL = "A(bl), A(bl)" -> "A(bl!1), A(bl!1)" :@ 1
  val bindBR = "A(br), A(br)" -> "A(br!1), A(br!1)" :@ 1
  val unbindLR = "A(r!1) , A(l!1) " -> "A(r) , A(l) " :@ 1
  val unbindBL = "A(bl!1), A(bl!1)" -> "A(bl), A(bl)" :@ 1
  val unbindBR = "A(br!1), A(br!1)" -> "A(br), A(br)" :@ 1

  //run
}

