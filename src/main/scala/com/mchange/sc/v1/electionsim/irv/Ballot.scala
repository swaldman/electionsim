package com.mchange.sc.v1.electionsim.irv

final case class Ballot( candidates : List[String] ) {
  require( candidates.toSet.size == candidates.size, s"Candidate list ${candidates} contains duplicates, shouldn't." )
}
