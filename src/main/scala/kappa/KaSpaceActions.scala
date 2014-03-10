package kappa

import scala.language.implicitConversions

import scala.collection.mutable


trait KaSpaceActions extends KappaLikeActions {
  this: KaSpaceContext
      with SiteGraphs
      with ContactGraphs
      with Patterns
      with Mixtures
      with RollbackMachines
      with Embeddings
      with PartialEmbeddings
      with Rules =>

  /** Factory object for building KaSpace actions.  */
  implicit object KaSpaceActionBuilder extends ActionBuilder {

    type RuleBuilder = KappaLikeRuleBuilder

    /**
     * Construct a KaSpace action from a LHS and RHS pattern using the
     * longest-common-prefix rule.
     *
     * @param lhs the left-hand side of this action.
     * @param rhs the right-hand side of this action.
     */
    def apply(lhs: Pattern, rhs: Pattern): RuleBuilder = {

      val (pe, rhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(lhs, rhs)

      // Build the action
      new KappaLikeRuleBuilder(new Action(lhs, rhs, pe,
        rhsAgentOffsets, None,
        Some(Geometry.checkGeometricSoundnessPostCondition)))
    }
  }

  /** Factory object for building bidirectional KaSpace actions.  */
  implicit object KaSpaceBiActionBuilder extends BiActionBuilder {

    type BiRuleBuilder = KappaLikeBiRuleBuilder

    def apply(lhs: Pattern, rhs: Pattern): BiRuleBuilder = {

      val (fwdPe, rhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(lhs, rhs)

      val (bwdPe, lhsAgentOffsets) =
        KappaLikeActionBuilder.commonLongestPrefix(rhs, lhs)

      new KappaLikeBiRuleBuilder(new BiAction(lhs, rhs, fwdPe, bwdPe,
        lhsAgentOffsets, rhsAgentOffsets, None,
        Some(Geometry.checkGeometricSoundnessPostCondition)))
    }
  }

  object Geometry {
    def checkGeometricSoundnessPostCondition(
      action: Action, inj: Action.Injection): Boolean =
      if (inj.isEmpty) true
      else {

        // println("# post-condition after action = " + action.lhs +
        //   " -> " + action.rhs + " (" + action + ")")

        // Get target mixture
        val mix = inj.head.mixture

        // Find all agents that have been updated _and_ that are
        // part of the RHS of the rule (other agents can not
        // possibly affect soundness).
        val toCheck = action.rhsAgentOffsets.values map inj filter
          (_ hasMark Updated)

        checkGeometricSoundness(toCheck)
      }

    /**
     * Check the geometric soundness of the smallest set of components
     * of the mixture containing all the agents in `toCheck`.
     */
    def checkGeometricSoundness(toCheck: Iterable[Mixture.Agent]): Boolean = {

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
          val as = new mutable.ArrayBuffer[Mixture.Agent]
          // val cg = checkGeometry(u, as)
          // val cc = checkCollisions(as.toArray)
          // println("# geom soundness = " + cg)
          // println("# coll soundness = " + cc)
          // cg && cc
          checkGeometry(u, as) && checkCollisions(as.toArray)
        }
      }
    }

    /**
     * Check the soundness of a connected component recursively
     * through a depth-first traversal starting at a given agent.
     */
    def checkGeometry(
      u: Mixture.Agent, as: mutable.Buffer[Mixture.Agent]): Boolean = {
      mix.mark(u, Visited)
      as += u
      val up = u.state.position
      val uo = u.state.orientation
      u.indices forall { i =>
        (u.siteStates(i).position, u.links(i)) match {
          case (Some(sp), Mixture.Linked(
            v, j, _, KaSpaceLinkState(_, Some(lo), _))) => {

            // (v: u.LinkTarget).siteStates(j).position match {
            // (v: KaSpaceActions.this.Mixture.Agent).siteStates(j).position match {
            v.siteStates(j).position match {
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

    /**
     * Check the agents in `as` for collisions.
     *
     * FIXME: Naive quadratic collision detection algorithm...
     */
    def checkCollisions(as: Array[Mixture.Agent]): Boolean = {
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
  }
}

