package at.forsyte.harrsh.entailment.learning

/**
  * Created by jkatelaa on 5/26/17.
  */
trait LearningComponent {

  val learningLog : EntailmentLearningLog = new EntailmentLearningLog

  def componentDescriptions : Seq[String] = Seq()

}
