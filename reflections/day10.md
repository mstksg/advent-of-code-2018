I originally did this by running a simulation, parting the velocity and points
into two lists and using `zipWith (+)` for the simulation.  However, I found a
much nicer closed-form version that [I wrote about in my blog][d10b]!
