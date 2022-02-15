package Restanta

import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Flip, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.{VariableElimination, MPEVariableElimination}
import com.cra.figaro.algorithm.sampling.{Importance, MetropolisHastings, ProposalScheme, MetropolisHastingsAnnealer, Schedule}
import com.cra.figaro.algorithm.factored.beliefpropagation.{BeliefPropagation, MPEBeliefPropagation}
import com.cra.figaro.algorithm.factored.beliefpropagation.MPEBeliefPropagation
import com.cra.figaro.algorithm.OneTimeMPE

//1. Să se definească o clasă abstractă care modelează structura unei stări. Elementul
//pentru modelarea vânzărilor poate fi definit aici.
//2. Să se definească o clasă care modelează starea inițială.
//3. Să se definească o clasă care modelează următoarea stare în funcție de starea curentă.
//4. Să se definească o secvență de stări corespunzătoare unui an. Se va scrie o funcție
//care creează secvență de lungime dată ca o listă (se va folosi operatorul “:+” pentru a
//adăuga un element la sfârșitul listei).
//5. Observând că în primele 2 luni vânzările au avut un nivel crescut și că în luna a treia un
//nivel mediu, să se prezică evoluția prețurilor pentru luna a patra, utilizând algoritmii MPE
//Variable Elimination, MPE Belief Propagation și Metropolis Hasting Annealer. Comparati
//valorile și adăugați sub formă de comentarii cele observate.
//6. Observând că în lunile 4 și 5 vânzările au stagnat, să se calculeze probabilitățile ca
//prețurile să fi scăzut în lunile 2 și 3
//7. Observând că în în primele 3 luni vânzările au avut un nivel mediu iar în luna a patra un
//nivel foarte scăzut, care este cea mai probabilă cauză din luna a treia ce a dus la
//această scădere.






object ex1
{
	abstract class State
	{
		val pret = Select( 0.4-> 'creste, 0.2 -> 'stagneaza, 0.4 -> 'scade)

		 def vanzare: (Element[Boolean]) = {
		CPD( pret,
			('creste) -> Flip(0.4),
			('stagneaza) -> Flip(0.2),
			('scade) -> Flip(0.4)

		 }

	}

	class InitialState() extends State
	{
		val pret = Constant('creste)
	}

	class NextState(current: State) extends State
	{
		val pret =  CPD(current.pret,
			creste ->Select(0.3 -> 'creste, 0.6 -> 'stagneaza, 0.1 -> 'scade) ,
			stagneaza -> Select(0.2 -> 'creste, 0.6 -> 'stagneaza, 0.2 -> 'scade),
			scade -> Select(0.3 -> 'creste, 0.1 -> 'stagneaza, 0.6 -> 'scade)
	}



	def stateSequence(n: Int): List[State] =
	{
		if (n == 0)
			List(new InitialState())
		else
		{
			val last :: rest = stateSequence(n - 1)
			new NextState(last) :: last :: rest
		}
	}

	def run(obsSeq: List[Boolean], algorithm:OneTimeMPE)
	{
		val stateSeq = stateSequence(obsSeq.length)
		for { i <- 0 until obsSeq.length }
		{
			stateSeq(i).vanzare.observe(obsSeq(obsSeq.length - 1 - i))
		}

		algorithm.start()
		for { i <- 0 until stateSeq.length - 1 }
		{
			print(obsSeq(obsSeq.length - 1 - i))
			print("\t")
			println(algorithm.mostLikelyValue(stateSeq(stateSeq.length - 1 - i).pret))
		}

		algorithm.kill()
	}


	def main(args: Array[String])
	{
		var steps = 12
		var obsSeq = List.fill(steps)(scala.util.Random.nextBoolean())

		println("MPE variable elimination")
		run(obsSeq, MPEVariableElimination())
		println("MPE belief propagation")
		run(obsSeq, MPEBeliefPropagation(12))
		println("Simulated annealing")
		run(obsSeq, MPEBeliefPropagation(2))
		println("Simulated annealing")
		run(obsSeq, MetropolisHastingsAnnealer(100000, ProposalScheme.default, Schedule.default(1.0)))


	}
}