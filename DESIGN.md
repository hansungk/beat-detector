## Structure of dynne's chunks

((double[10000] left, double[10000] right), ... , (double[<=10000] left, double[<=10000] right))

Total length of all double arrays (1 channel) : duration(s) * samplingrate(Hz)

## Refreshing history buffer

History buffer  <-  Raw sample data
( 44032 samples ) <- ( 10000 samples ) ( 10000 samples ) ...

* Passing all raw data is too heavy, pass only index instead
* The history buffer should be refreshed after every step (every 'instance')
* Use of drop and conj?

Call a func with buffer -> func returns beat detection result -> call func again with updated hist buffer

# Performance

* into : Vector > Cons
* From Java array conversion : Seq == List > Lazy-seq > Vector
