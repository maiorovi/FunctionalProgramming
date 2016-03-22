def maybeTwice(condition:Boolean, action: => Int) = if(condition) action+action else 0

maybeTwice(true, {println("hi"); 41+1})

// it`s obvious now that parameters like action: => Int behaves like we passing a
// function of one argument to it action: () => Int

// we can cache the the given value explicitly if we want to evaluate expression only
// once

def maybeTwice2(condition:Boolean, action: => Int) = {
  lazy val j = action
  if (condition) j+j else 0
}

maybeTwice2(true, {println("hi"); 41+1})