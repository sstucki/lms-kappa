package kappa

import scala.language.implicitConversions

import scala.collection.mutable

trait KaSpaceActions extends Actions {
  this: KaSpaceContext with Patterns with Mixtures with Embeddings
      with PartialEmbeddings with Rules =>

  /** Factory object for building KaSpace actions.  */
  object KaSpaceAction {

    /**
     * Construct a KaSpace action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     */
    def apply(lhs: Pattern, rhs: Pattern): Action = {

      import Pattern._

      // Compute the offsets of the first agents of each component of
      // the LHS in the agents array passed to an action application.
      val ceOffsets: Vector[Int] =
        lhs.components.scanLeft(0) { (i, ce) => i + ce.length }

      @inline def lhsAgentOffset(a: Agent) =
        ceOffsets(a.component.index) + a.index

      // Find the longest common prefix, i.e. longest prefix in the
      // sequences of agents making up the LHS and RHS, for which the
      // number of sites and the agent states match up.
      val zipped = (lhs zip rhs)
      val firstDiffIndex = zipped indexWhere {
        case (la, ra) =>
          !((la.sites.length == ra.sites.length) &&
            (la.state matchesInLongestCommonPrefix ra.state))
      }
      val commonPrefixLength =
        if (firstDiffIndex > 0) firstDiffIndex else zipped.length
      val longestCommonPrefix = zipped take commonPrefixLength

      // Construct the partial embedding corresponding to this action
      val pe = PartialEmbedding(
        lhs take commonPrefixLength, rhs take commonPrefixLength)

      // Compute the RHS agent offsets
      val rhsPrefixAgentOffsets =
        for (i <- 0 until commonPrefixLength)
        yield (rhs(i), lhsAgentOffset(lhs(i)))
      val rhsSuffixAgentOffsets =
        for (i <- commonPrefixLength until rhs.length)
        yield (rhs(i), lhs.length + i - commonPrefixLength)
      val rhsAgentOffsets =
        (rhsPrefixAgentOffsets ++ rhsSuffixAgentOffsets).toMap

      def checkGeometricSoundnessPostCondition(
        action: Action, agents: Action.Agents): Boolean =
        if (agents.isEmpty) true
        else {

          // println("# post-condition after action = " + action.lhs +
          //   " -> " + action.rhs + " (" + action + ")")

          // Get target mixture
          val mix = agents.head.mixture

          // Find all agents that have been updated _and_ that are
          // part of the RHS of the rule (other agents can not
          // possibly affect soundness).
          val toCheck = for {
            i <- action.rhsAgentOffsets.values
            if (agents(i) hasMark Updated) // all marked agents have been updated
          } yield agents(i)

          checkGeometricSoundness(toCheck)
        }

      // Build the action
      new Action(lhs, rhs, pe, rhsAgentOffsets, None,
        Some(checkGeometricSoundnessPostCondition))
    }

    /**
     * Check the geometric soundness of the smallest set of components
     * of the mixture containing all the agents in `toCheck`.
     */
    def checkGeometricSoundness(toCheck: Iterable[Mixture.Agent]): Boolean = {

      import Mixture._

      // Check the soundness of a connected component recursively
      // through a depth-first traversal starting at a given agent.
      def checkGeometry(u: Agent, as: mutable.Buffer[Agent]): Boolean = {
        mix.mark(u, Visited)
        as += u
        val up = u.state.position
        val uo = u.state.orientation
        u.sites forall { s =>
          (s.state.position, s.link) match {
            case (Some(sp), Linked(v, j, KaSpaceLinkState(_, Some(lo)))) => {
              val t = v.sites(j)
              t.state.position match {
                case Some(tp) => {
                  val vo = lo * uo
                  val vp = up + uo * sp - vo * tp
                  if (v hasMark Visited) {
                    // Check previously computed position and orientation
                    (v.state.position ~= vp) && (v.state.orientation ~= vo)
                  } else {
                    // Assign new position and orientation
                    v.state = v.state.copy(position = vp, orientation = vo)
                    checkGeometry(v, as)
                  }
                }
                case None => true
              }
            }
            case _ => true
          }
        }
      }

      // Check the agents in `as` for collisions. FIXME: Naive quadratic
      // collision detection algorithm...
      def checkCollisions(as: Array[Agent]): Boolean = {
        as.indices forall { i =>
          (i + 1) until as.size forall { j =>
            val u = as(i)
            val v = as(j)
              (u.state.radius, v.state.radius) match {
              case (Some(ru), Some(rv)) => {
                val d1 = ru + rv
                val d2 = u.state.position - v.state.position
                //println("d1^2: " + (d1 * d1) + ", d2^2: " + (d2 * d2))
                d1 * d1 <= d2 * d2
              }
              case _ => true
            }
          }
        }
      }

      // Clear the marked agents so we can use the mark/unmark
      // mechanism to keep track of agents that have already been
      // updated.
      mix.clearMarkedAgents(Visited)

      // Iterate over all connected components by iterating over all
      // the agents whose position/orientation has not been updated
      // yet and using them as root agents for a depth-first
      // traversal.
      toCheck forall { u =>
        (u hasMark Visited) || {
          val as = new mutable.ArrayBuffer[Agent]
          // val cg = checkGeometry(u, as)
          // val cc = checkCollisions(as.toArray)
          // println("# geom soundness = " + cg)
          // println("# coll soundness = " + cc)
          // cg && cc
          checkGeometry(u, as) && checkCollisions(as.toArray)
        }
      }
    }
  }

  /** Convert a pair `(lhs, rhs)` of patterns into a KaSpace action. */
  implicit def patternPairToKaSpaceAction(lr: (Pattern, Pattern)): Action =
    KaSpaceAction(lr._1, lr._2)

  /**
   * Convert a pair `(lhs, rhs)` of pattern strings into a KaSpace
   * action.
   */
  implicit def stringPairToKaSpaceAction(lr: (String, String)): Action =
    KaSpaceAction(Pattern(lr._1), Pattern(lr._2))
}

