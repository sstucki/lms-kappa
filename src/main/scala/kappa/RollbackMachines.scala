package kappa

trait RollbackMachines {
  this: LanguageContext with SiteGraphs with Patterns with Mixtures =>

  import SiteGraph.{Link, Stub}
  import Mixture.{Agent, Linked}

  trait RollbackMachine {
    this: Mixture =>

    /**
     * A class representing a single mixture checkpoint.
     *
     * FIXME: Should we use [[Mixture]] for this?
     */
    final class Checkpoint(
      val head: Agent, val length: Int,
      var agents: List[Agent])

    /** The stack of mixture checkpoints. */
    private var _checkpoints: List[Checkpoint] = Nil

    /**
     * Make a checkpoint of this mixture.
     *
     * After a call to this method, every update to an agent of this
     * mixture through the any of the [[Mixture]]'s methods will cause
     * the corresponding agent to be backed up.  The mixture may be
     * reset to its state prior to the call to this method by calling
     * [[Mixture.rollback]].
     *
     * Mixture checkpoints are stored on a stack, i.e. in FIFO order.
     * Hence one may chose to call this method multiple times before
     * calling [[Mixture.rollback]].  Each such call will result in a
     * new checkpoint.
     *
     * NOTE: this method will not copy the agent, site and link state
     * types (represented by [[LanguageContext#AgentState]],
     * [[LanguageContext#SiteState]] and
     * [[LanguageContext#LinkState]]) when creating a checkpoint.
     * Instead, the checkpoint will just contain references to such
     * classes.  If these classes contain mutable state, updates to
     * that state will be reflected in the checkpoint and can not be
     * reverted through a call to [[Mixture.rollback]].
     *
     * NOTE 2: The lift maps of an agents are not checkpointed.
     *
     * @return this mixture.
     */
    def checkpoint: Mixture = {
      _checkpoints = (new Checkpoint(_head, _length, Nil)) :: _checkpoints
      this
    }

    /**
     * Restore the state of this mixture to the last checkpoint.
     *
     * See [[Mixture.checkpoint]] for details.
     *
     * @return this mixture.
     */
    def rollback: Mixture = {
      if (_checkpoints.isEmpty) throw new IllegalStateException(
        "attempt to roll back mixture with empty checkpoint stack")

      // Roll back all the agents in the checkpoint.
      val cp = _checkpoints.head
      for (v <- cp.agents) {
        val u = v.copy

        // Restore the state of `u` to that of `v`.
        u.state = v.state
        for (i <- v.indices) {
          u._siteStates(i) = v._siteStates(i)
          u._links(i) = v._links(i)
        }
        u.prev = v.prev
        u.next = v.next
        if (u.prev != null) u.prev.next = u
        if (u.next != null) u.next.prev = u
        //mark(u, Updated)
      }

      // Restore the head and length fields of the mixture.
      _head = cp.head
      _length = cp.length

      // Remove the checkpoint from the top of the stack.
      _checkpoints = _checkpoints.tail
      this
    }

    /**
     * Discard the latest checkpoint of this mixture.
     *
     * See [[Mixture.checkpoint]] for details.
     *
     * @return this mixture with its last checkpoint discarded.
     */
    def discardCheckpoint: Mixture = {
      _checkpoints = _checkpoints match {
        case _ :: cps => cps
        case Nil => throw new IllegalStateException(
          "attempt to discard checkpoint of mixture with empty " +
            "checkpoint stack")
      }
      this
    }

    /**
     * Make a copy of this agent and add it to the current checkpoint.
     *
     * If the checkpoint stack is empty or if the agent is marked, no
     * copy will be created.  If a copy is created, its `copy` field
     * will point to the original agent `u`.
     *
     * @param u the agent to add to the current checkpoint.
     */
    @inline protected def checkpointAgent(u: Agent) {
      if (!_checkpoints.isEmpty && !(u hasMark Updated)) {
        val v = u.checkpointCopy
        val cp = _checkpoints.head
        cp.agents = v :: cp.agents
      }
    }
  }

  trait RollbackAgent {
    this: Agent =>

    /**
      * Make a checkpoint copy of a given agent.
      *
      * @return a copy of this agent (sharing states and links).
      */
    protected[kappa/*Mixture*/] def checkpointCopy: Agent = {
      // Allocate an agent `v` tracking `this`.
      val v = new Agent(
        this.state, this._siteStates.clone, this._links.clone)
      v._mixture = this._mixture
      v.prev = this.prev
      v.next = this.next
      v._copy = this
      v
    }
  }
}
