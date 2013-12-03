%include polycode.fmt

This version aims to mirror the specification. It is not nice Haskell!

> module DCA where

> import Data.List (nub)

> data Stream a = a :<: (Stream a)

> type Time = Double

We assume that we are using the 2-signal model such that $\lambda=2$ for a system where the events (antigens) are process IDs. We represent process IDs using integers:

> type E = Int

each signal stream has it's own time data, but given that signals are already synchronised, shouldn't it be:

> type S = Stream (Time, Double, Double) 

> stime :: (Time, Double, Double)  -> Time
> stime (t,_,_) = t

 type S = (Stream (Double, Time), Stream (Double, Time))

> type A = Stream (E, Time)

> atime :: (E, Time) -> Time
> atime = snd 

> type AnomalyScore = (E, Time)

a list of anomaly scores is easier to implement; normally this should be of a fixed size with one field one for each element in E

> type Theta = [AnomalyScore]
> type Phi   = [((E,Time), Time)]

> type Sigma = [(E, Time)]

rethink what needs to be $\mathbb{R}$ and what needs to be $\mathbb{N}$?

> type Omega = (Double, Double, Double)

> type Cell = (Sigma, Omega, Double)

> new :: Double -> Cell
> new t = ([], (0.0, 0.0, 0.0), t)

> dead :: Cell -> Bool
> dead (_,(_,_,om),ts) = om >= ts

> reset :: Cell -> Cell
> reset (_,_,ts) = new ts

> events :: Cell -> Sigma
> events (es,_,_) = es

> score :: Cell -> Double
> score (_,(oa,oi,_),_) = oa - oi

Maybe we should rename N to Pop?

> type N = [Cell]

> type H = (A, S, N)

> n :: Int
> n = 29197 

> initPop :: N
> initPop = [new i | i <- [1.0..100.0]]

> dca :: (A,S) -> Theta
> dca = analyse . run

> run :: (A,S) -> Phi
> run (a,s) = process ((a,s,initPop),0)

process' and updateS are currently a horrible mess. I will make them nicer later. especially the signal update :)

> process :: (H,Int) -> Phi
> process (h,i)
>   | i > n             = termResults h
>   | otherwise         = r ++ process(h',i+1)
>                           where
>                               (r, h') = update h

> update :: H -> (Phi,H)
> update (a :<: as, s :<: ss, pop)
>   | atime a < stime s = ([], (as, s :<: ss, updateA (a,pop)))
>   | otherwise         = (r,  (a :<: as, ss, pop''))
>                           where
>                               pop'  = updateS (s,pop)
>                               r     = results pop'
>                               pop'' = migrate pop'

> updateA :: ((E, Time), N) -> N
> updateA (e,((es,os,ls):pop)) = pop ++ [(e:es,os,ls)]

> updateS :: ((Time, Double, Double), N) -> N
> updateS ((_, s0, s1), pop) = [(es, accumulate (os, transduction (s0,s1)), ts) | (es,os,ts) <- pop]

> transduction :: (Double, Double) -> Omega
> transduction (s0, s1) = (s0 + s1 * (-2),s1,s0 + s1)

> accumulate :: (Omega, Omega) -> Omega
> accumulate ((oa,oi,om),(oa',oi',om')) = (oa+oa',oi+oi',om+om')

> results :: N -> Phi
> results pop = [(e, score c) | c <- pop, dead c, e <- events c]

> migrate :: N -> N
> migrate pop = [if dead c then reset c else c | c <- pop]

> termResults :: H -> Phi
> termResults (_,_,pop) = [(e, score c) | c <- pop, e <- events c]

> analyse :: Phi -> Theta
> analyse as = [(e, avg (e, as)) | e <- unique as]

The following two functions are required for the implementation. The first is a helper function to find the list of all event IDs. This is required since E is defined as Int and it would not make sense to iterate through all possible values for that:

> unique :: Phi -> [E]
> unique = nub . map (fst . fst)

The second is needed to resolve the scoping issue with 'where':

> avg :: (E, Phi) -> Double
> avg (e, as) = sum vs / fromIntegral (length vs) 
>   where
>       vs = [v | ((e',t),v) <- as, e == e']
