import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.Map

class QuadBlock(val store : scala.collection.mutable.Map[Int, Set[Int]], val start : Int, val end : Int, descendants : Int) {	
	val children : Map[Char, QuadBlock] = if (descendants > 0) {
		val sz = (end-start)/4
		Map('a' -> new QuadBlock(store, start, start+sz, descendants-1),
				'b' -> new QuadBlock(store, start+sz, start+sz*2, descendants-1),
				'c' -> new QuadBlock(store, start+sz*2, start+sz*3, descendants -1),
				'd' -> new QuadBlock(store, start+sz*3, end, descendants -1))
	} else {
		Map()
	}
	
	def this(store : scala.collection.mutable.Map[Int, Set[Int]], descendants : Int) = this(store, 0, store.size, descendants)
	
	def connectionsTo(other : QuadBlock) : Int = {
		var r = 0
		for (p <- start until end) {
			val refs = store(p)
			for (ref <- refs) {
				if (other.contains(ref)) {
					r += 1
				}
			}
		}
		r
	}
	
	def contains(p : Int) : Boolean = {
		p >= start && p < end
	}
	
	def swap(a : Int, b : Int) = {
		println("Swap "+a+" and "+b)
		val storeClone = store.clone()
		for ((key, value) <- storeClone) {
			store.update(key, value map (ref => if (ref == a) b else if (ref == b) a else ref)
			)
		}
		store.update(a, storeClone(b))
		store.update(b, storeClone(a))
	}

	def crossCount = {
		if (children.isEmpty) (0, 0) else {
			val grid = new HashMap[(Char, Char), Int]()
			for (
					a <- List('a', 'b', 'c', 'd'); b <- List('a', 'b', 'c', 'd')
			) {
				if (a != b) {
					grid.put((a, b), children(a).connectionsTo(children(b)))
				}
			}
			val now = grid(('a', 'c'))+grid(('a', 'd'))+grid(('b', 'c'))+grid(('b', 'd'))+
			grid(('c', 'a'))+grid(('d', 'a'))+grid(('c', 'b'))+grid(('d', 'b'))
			val afterACswap = grid(('c', 'a'))+grid(('c', 'd'))+grid(('b', 'a'))+grid(('b', 'd'))+
			grid(('a', 'c'))+grid(('d', 'c'))+grid(('a', 'b'))+grid(('d', 'b'))
			(now, afterACswap)
		}
	}

	def optimize : Unit = {
		///println("Checking "+start+"-"+end)
		if (!children.isEmpty) {
			children.values.foreach (_.optimize)
		}
		val (now, afterACswap) = crossCount
		//println(now +" -> "+afterACswap)
		if (afterACswap < now) {
			println("Swapping in "+start+"-"+end+" to reduce "+now+" to "+afterACswap)
			for ((ap, cp) <- (children('a').start until children('a').end) zip (children('c').start until children('c').end)) {
				swap(ap, cp)
			}
		}

	}
}
