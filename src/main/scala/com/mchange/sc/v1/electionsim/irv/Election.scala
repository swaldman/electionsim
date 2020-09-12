package com.mchange.sc.v1.electionsim.irv

import scala.collection._

final object Election {
  private val TallyOrdering = Ordering.by[Tuple2[String,Long],Tuple2[Long,String]]{ case(k,v) => (v,k) }.reverse

  /**
    * It's not clear what the right thing to do is when there is a tie among the losers and a new round must be computed.
    * By default, we just fail in this case, but you can replace this with the identity function (to just drop all tied losers)
    * or with some tie-breaking function that winnows down the set of losers if there are more than one.
    */ 
  private val DefaultLosersTransformer : immutable.SortedSet[String] => immutable.SortedSet[String] = { losers =>
    require( losers.size == 1, s"""Can't handle a tie among losers ${losers.mkString("[",", ","]")}. Invalid election, or define a custom losersTransformer to break or ignore the tie.""" )
    losers
  }
}
final case class Election( ballots : List[Tuple2[Ballot,Long]], losersTransformer : immutable.SortedSet[String] => immutable.SortedSet[String] = Election.DefaultLosersTransformer ) {
  lazy val votesCast = ballots.foldLeft(0L){ ( accum, next ) => accum + next._2 }

  lazy val pluralityTallies : immutable.SortedSet[Tuple2[String,Long]] = {
    val tallies = {
      ballots.foldLeft( immutable.Map.empty[String,Long] ){ ( accum, next ) =>
        val ( ballot, count ) = next
        ballot.candidates.headOption match {
          case Some( candidate ) => {
            val past = accum.get( candidate ).getOrElse( 0L )
            accum + Tuple2( candidate, past + count )
          }
          case None => accum
        }
      }
    }
    immutable.SortedSet( tallies.toSeq : _* )( Election.TallyOrdering )
  }

  lazy val ( pluralityWinners, pluralityWinCountPerCandidate, pluralityLosers, pluralityLoseCountPerCandidate ) = {
    if (pluralityTallies.nonEmpty) {
      val winCount = pluralityTallies.head._2
      val winners  = pluralityTallies.takeWhile{ case ( _, count ) => count == winCount }.map( _._1 )

      val loseCount = pluralityTallies.last._2
      if ( winCount == loseCount ) {
        ( winners, winCount, immutable.SortedSet.empty[String], 0L )
      }
      else {
        val losers = pluralityTallies.dropWhile{ case ( _, count ) => count > loseCount }.map( _._1 )
        ( winners, winCount, losers, loseCount )
      }
    }
    else {
      ( immutable.SortedSet.empty[String], 0L, immutable.SortedSet.empty[String], 0L )
    }
  }

  lazy val ballotsCast = ballots.foldLeft( 0L ) { ( accum, next ) => accum + next._2 }

  lazy val allCandidates = ballots.foldLeft( immutable.Set.empty[String] )( (accum, next) => accum ++ next._1.candidates )

  lazy val isDefinitive = pluralityWinners.isEmpty || (pluralityWinCountPerCandidate * 2) > ballotsCast || pluralityWinners.size == allCandidates.size

  lazy val next = {
    if (isDefinitive) {
      None
    }
    else {
      val nextBallots = {
        val effectiveLosers = losersTransformer( pluralityLosers )
        ballots.map { case ( ballot, count ) => ( Ballot( ballot.candidates.filter( c => !effectiveLosers(c) ) ), count ) }
      }
      Some( Election( nextBallots ) )
    }
  }

  lazy val irvWinners : immutable.SortedSet[String] = {
    next match {
      case Some( election ) => election.irvWinners
      case None             => pluralityWinners
    }
  }
}
