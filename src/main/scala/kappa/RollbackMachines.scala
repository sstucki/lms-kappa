package kappa

trait RollbackMachines {
  this: LanguageContext
      with ContactGraphs
      with SiteGraphs
      with Mixtures
      with Patterns
      with ComponentEmbeddings =>

  import SiteGraph.{Link, Stub}
  import Mixture.{Agent, AgentImpl, Linked}

  trait RollbackMachine {
    this: Mixture =>

    /** A class representing a single mixture checkpoint. */
    final class Checkpoint {

      val head: Agent = _head
      val length: Int = _length

      var agents = List[Agent]()

      var removedEmbeddings = List[ComponentEmbedding[Mixture.Agent]]()
      var addedEmbeddings = List[ComponentEmbedding[Mixture.Agent]]()
    }

    /** The stack of mixture checkpoints. */
    private var _checkpoints: List[Checkpoint] = List()

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
     */
    def checkpoint {
      _checkpoints = (new Checkpoint) :: _checkpoints
    }

    /**
     * Restore the state of this mixture to the last checkpoint.
     *
     * See [[RollbackMachine.checkpoint]] for details.
     *
     * @return this mixture.
     */
    def rollback: Mixture = {
      if (_checkpoints.isEmpty) throw new IllegalStateException(
        "attempt to roll back mixture with empty checkpoint stack")

      // Roll back all the agents in the checkpoint.
      val cp = _checkpoints.head
      for (v <- cp.agents) {
        // TODO: Isn't it simpler to just put `v` in place of `u`
        // by rewiring the mixture?
        val u = v._copy

        // Restore the state of `u` to that of `v`.
        u.state = v.state
        for (i <- v.indices) {
          u.siteStates(i) = v.siteStates(i)
          u.links(i) = v.links(i)
        }
        u.prev = v.prev
        u.next = v.next
        if (u.prev != null) u.prev.next = u
        if (u.next != null) u.next.prev = u
        // mark(u, Updated)
      }

      for (ce <- cp.removedEmbeddings) {
        ce.component.addEmbedding(ce)
        for (i <- ce.indices)
          ce(i).addLift(ce.component(i), ce)
      }

      for (ce <- cp.addedEmbeddings) {
        ce.component.removeEmbedding(ce)
        for (i <- ce.indices)
          ce(i).removeLift(ce.component(i))
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
     * See [[RollbackMachine.checkpoint]] for details.
     *
     * @return this mixture with its last checkpoint discarded.
     */
    def discardCheckpoint: Mixture = {
      _checkpoints = _checkpoints match {
        case _ :: cps => cps
        case List() => throw new IllegalStateException(
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
    @inline // protected
    def checkpointAgent(u: Agent) {
      // RHZ: Why no copy should be created when marked?
      if (!_checkpoints.isEmpty && !(u hasMark Updated)) {
        val v = u.checkpointCopy
        val cp = _checkpoints.head
        cp.agents = v :: cp.agents
      }
    }

    def checkpointRemovedEmbedding(
      ce: ComponentEmbedding[Mixture.Agent]) {
      _checkpoints match {
        case cp :: _ => cp.removedEmbeddings = ce :: cp.removedEmbeddings
        case List() => throw new IllegalStateException(
          "attempt to add embedding to checkpoint before checkpoint")
      }
    }

    def checkpointAddedEmbedding(
      ce: ComponentEmbedding[Mixture.Agent]) {
      _checkpoints match {
        case cp :: _ => cp.addedEmbeddings = ce :: cp.addedEmbeddings
        case List() => throw new IllegalStateException(
          "attempt to add embedding to checkpoint before checkpoint")
      }
    }
  }

  trait RollbackAgent {
    this: AgentImpl =>

    /**
      * Make a checkpoint copy of a given agent.
      *
      * @return a copy of this agent (sharing states and links).
      */
    protected[kappa/*Mixture*/] def checkpointCopy: AgentImpl = {
      // Allocate an agent `v` tracking `this`.
      // RHZ: What happens if this.state is modified?
      // Is the agent state shared with the copy?
      val v = new AgentImpl(this.agentType, this.state, this.siteTypes,
        this.siteStates.clone, this.links.clone, this.liftMap.clone)
      v._mixture = this._mixture
      v._copy = this
      v.prev = this.prev
      v.next = this.next
      v
    }
  }
}
