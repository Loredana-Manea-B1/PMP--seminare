package examen

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound.CPD


	class Album {
		val liked = Uniform(0, 0.1)

	class Autor(album: List[Album]) {
		private val likedContainer = Container(album.map(_.liked):_×)
		private val popularityMean = likedContainer.foldLeft(0.0)(_ max _)
		val popularity = Normal(popularityMean, 5)
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

		def getProb(): Unit ={

		}


	}




	val alg =
		MetropolisHastings(100000, ProposalScheme.default, melodia1.popularity)
	alg.start()
	println(alg.mean(melodia1.popularity))
	}