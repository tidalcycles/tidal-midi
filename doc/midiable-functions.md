

#Functions

Applictive functors work like this example: 

	# pan ((*) <$> sinewave1 <*> (slow 8 $ "0 0.25 0.75"))


In the above, the  `sinewave1`  and the  (slow 8 $ "0 0.25 0.75")  pattern are multiplied together. Using the <$>  and the  <*>  in this way turns the  *  operator, which normally works with two numbers, into a function that
instead works on two patterns of numbers.
