package scabs

final case class TCBenchVariety[C[_[_]], M[_]](name: String)
                                              (implicit val instance: C[M]) {
  def forget: TCBenchVariety[C, Nothing] =
    this.asInstanceOf[TCBenchVariety[C, Nothing]]
}

