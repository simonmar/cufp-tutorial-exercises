# 1. IO

## 1.1

Write a program that reads lines of text from standard input, and
outputs each line prefixed by its line number, starting from 1.  For
example, if the input is

    aaa
    bbb
    ccc

The output would be

    1 aaa
    2 bbb
    3 ccc

Here are some functions that you might find useful (you don't have to
use all of them, there are multiple ways to write this program).  Look
them up in the documentation:

    getLine, isEOF, putStr, getContents, lines, unlines

When you're done, hit the tick (check) icon in the top bar and commit
your files.

Create a file called "input" using the "Files" drop-down on the left,
put a few lines of text in it.

## 1.2

Modify your program to read this file as the input instead of the
standard input.  Functions you might need:

    readFile, openFile, hGetLine, hIsEOF

## 1.3

Modify the program to take an optional command-line argument.  To read
the command-line arguments, import `System.Environment` and call

    getArgs

When there are no arguments, the program should read from standard
input.  If there is a single argument, the program should read the
input from that file.

To run this program in two different ways, we'll need to use the
"Configurations" feature of the IDE.  Open the configurations tab by
pulling down "Main" at the top, and clicking "Edit" next to
"Configurations". Then create two configurations for your program: one
with no arguments, and one with a single argument "input", naming the
file that you created earlier.

Run the program in both configurations to make sure it does what you
expect: select the current configuration by pulling down the menu at
the top and selecting the desired configuration from the list.

Go ahead and use this basic pattern to build a few of the standard
Unix utilities:

* head (print out the first N lines of the input)
* tail (print out the last N lines of the input)
* sort (sort the lines of the input)
* uniq (remove adjacent duplicate lines)

# 2. Concurrency

## 2.1 Thread pools

A "thread pool" is an collection of threads that collaborate to run
jobs submitted via an API.  One use for a thread pool is when you want
to limit the amount of concurrency to a fixed number of threads,
rather than forking a thread for everything.  It's a useful little
exercise in concurrent programming.

The task is to write a thread pool.  This is the API:

    -- Creates the given number of threads, and returns a ThreadPool
    newThreadPool :: Int -> IO ThreadPool

    -- Submit a job of type IO () to the ThreadPool.  One of the
    -- threads in the thread pool should pick up the job and run
    -- it.  submit doesn't wait for the result, it should return
    -- immediately
    submit  :: Pool -> IO () -> IO ()

Write a main function that submits 10 jobs to a thread pool of size 3,
with each one printing its number.  Use mapM or forM to submit the
jobs.

Hints:

* use `data ThreadPool = ThreadPool (MVar (IO ()))`

* don't forget to make your worker threads loop to pick up the next job after completing one.  `forever` is useful here.

* You might need to use threadDelay at the end of the program to wait for the jobs to finish.

## 2.2

Add some debugging output so you can see what's going on.  We'll
need

    myThreadId :: IO ThreadId

and you can convert a ThreadId to a String using 'show'.

When a worker thread picks up a job from the pool, make it print out a
message containing its ThreadId, so you can see which threads are
running.

Modify the submit operation:

    submit :: Pool -> IO Int -> IO Job

and add a new operation:

    wait :: Job -> IO Int

So now a job can return a value, and we can wait for the value at any
time.

Test this by modifying your program to wait for the jobs after
submitting them.

## 2.3

Bonus question (feel free to skip to 2.4).

Try this main function with your thread pool:

    main = do
      p <- newThreadPool 3
      let run io = do j <- submit p io; wait j
          end = do print "DONE"; return 0

      run $ run $ run end

It should print "DONE".  Now add one more run, like this:

      run $ run $ run $ run end

What happens?  Can you explain why? (feel free to add more
debugging output to help you understand what's going on).

What does this tell us about thread pools?

## 2.4

Write a game that works as follows:

The program prints out an arithmetic problem of the form

    X op Y

where X and Y are numbers between 0 and 50, and op is either +, -, or
*.

The user has 10 seconds to type the answer.  If they enter the correct
answer within the time limit, the response is "CORRECT", if they
answer wrongly the response is "WRONG", if the time runs out the
response is "OUT OF TIME".  The program then exits.

Some things you might need:

    -- binds x to a random number between 0 and 50
    x <- randomRIO (0,50::Int)

    -- sleep for a while
    threadDelay :: Int -> IO () -- Int is in microseconds

    -- convert a String into another type (e.g. Int)
    read :: Read a => String -> a


# 3. Channels

# 4. Overlapping I/O

Clone the repo at

   http://community.haskell.org/~simonmar/bingtranslator.git

This is a program to translate some text into various different
languages using the Bing translation API.  The translation API is free
to use up to a certain number of requests per day; I have signed up
for a key and included it in the code, so all you need to do is use
the API provided by the BingTranslator module.

There's a simple Main module that does the followin:

1. reads a line of text from the console
2. determines what language it is in
3. asks the API for the list of languages it supports,
4. and then translates the text into all the other languages

## 3.1

Your first task is to perform (2) and (3) concurrently.  Either use
MVar directly, or the simple Async API from the slides.

## 3.2

Secondly, perform all the translations in step (4) concurrently.

## 3.3

You might find that this doesn't work well - the API seems to have a
limit on the number of concurrent requests, so if you submit them all
together, some will time out.  So the next task is to ensure that we
limit the number of requests we make at any one time - use a `QSem` to
limit the number of concurrent connections.  Start with 3 concurrent
connections, and vary it up and down to see what effect it has.
