package examen

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound.CPD

import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.sampling.{MetropolisHastingsAnnealer, Schedule, _}
import com.cra.figaro.language.Universe.universe
import com.cra.figaro.language.{Flip, _}
import com.cra.figaro.library.compound.If

	object examen{

		abstract class Autor() {
		private val likedContainer = Container(album.map(_.liked):_×)
		private val popularityMean = likedContainer.foldLeft(0.0)(_ max _)
		val popularity = Normal(popularityMean, 5)
			val confident: Element[Boolean]
			def possession: Element[Boolean] =
				If(confident, Flip(0.1), Flip(0.5))
	}

		class InitialAutor() extends Autor {
			val confident = Flip(0.4)
		}

		class NextAutor(current: State) extends Autor {
			val confident = If(current.confident, Flip(0.6), Flip(0.3))
		}
		// produce a state sequence in reverse order of the given length

		def AutorSequence(n: Int): List[Autor] = {
			if (n == 0)
				List(new InitialAutor())
			else {
				val last :: rest = autorSequence(n - 1)//add la inceputul listei un element ::
				new  NextAutor(last) :: last :: rest
			}
		}


		def run(algorithm: OneTimeMPE) {
			val obsSeq: List[Boolean] = List.fill(10)(scala.util.Random.nextBoolean())
			val stateSeq = autorSequence(obsSeq.length)


			for {i <- obsSeq.indices} {
				//facem observatiile
				stateSeq(i).possession.observe(obsSeq(obsSeq.length - 1 - i))
				print("\t")
			}

			//pornim algoritmul , DUPA ce am facut observatiile
			algorithm.start()


		}


		class Album {
			val liked = Uniform(0, 0.1)
		}


	val album = Array.fill(10)(new Autor)
	val melodia1 = new Melodia(List(autor(0), autor(1)))
	val melodia2 = new Melodia(List(autor(0), autor(1)))
	val melodia3 = new Melodia(List(autor(0), autor(1)))



	def constrain(melodie: Album, pop: Double) {
		movie.popularity.addConstraint(d =>
			math.exp(-(pop-d)×(pop-d)/0.01))
	}
	constrain(melodia1, 0.27)
	constrain(melodia2, 0.6)
	constrain(melodia3, 0.13)

	class Nominalizare(album: Album){




	}




	val alg =
		MetropolisHastings(100000, ProposalScheme.default, melodia1.popularity)
	alg.start()
	println(alg.mean(melodia1.popularity))
	}
